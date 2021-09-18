/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-07-09
  * Description: Decoder.
  */
package megumi32.rtl

import chisel3._
import chisel3.Bool
import chisel3.util._
import megumi32.include._
import megumi32.include.CtrlOpConstans._
import megumi32.include.ALUOp._
import megumi32.include.MULop._
import megumi32.include.FPU._
import megumi32.include.APU._
import megumi32.include.CSROp._
import megumi32.include.Opcode._
import megumi32.include.FPOpGroup._
import megumi32.include.FPOpLatency._
import megumi32.include.FPExtenConfi._

abstract trait DecodeTable {
    val table: Array[(BitPat, List[UInt])]
}

class IDecode extends DecodeTable {
    val table: Array[(BitPat, List[UInt])] = Array(

    )
}

class DecoderIO()(p: IDParams) extends Bundle {
    val deassertWe           = Input(Bool())

    val illegalInsn         = Output(Bool())                // Illegal instruction encountered
    val eBrkInsn            = Output(Bool())                // Trap instruction encountered

    /** Return from exception/debug instruction encountered & without deassert version */
    val retInsn             = Output(new RetInsn()(p))

    val eCallInsn           = Output(Bool())                // Environment call (syscall) instruction encountered
    val wfi                 = Output(Bool())                // Pipeline flush is requested

    val fenceiInsn          = Output(Bool())                // FENCE.I instruction

    /** Registers used signals */
    val regUsed             = Output(new RegUsed()(p))

    //from IF/ID pipeline
    val insnRdata       = Input(UInt(p.XLEN.W))
    val illegalCInsn    = Input(Bool())


    /** Control signals */
    val ctrlSig             = new CtrlSig()(p)

    val currentPrivLvl      = Input(UInt(PrivLvl.width.W))  // The current privilege level

    val debugMode           = Input(Bool())                 // Processor is in debug mode
    val debugWfiNoSleep     = Input(Bool())                 // Do not let WFI cause sleep

    /** Jump/Branches */
    val ctrlTransferInsnInDec = Output(UInt(2.W))           // Control transfer instruction without deassert
    val ctrlTransferInsnInID  = Output(UInt(2.W))           // Control transfer instruction is decoded
    val ctrlTransferTargetMuxSel = Output(UInt(2.W))        // Jump target selection

    val mcounteren            = Input(UInt(32.W))           // Jump target selection

    /** Decode Output Signals */ 
    //                            |              MISC Signals              | |                                      ALU Signals                                          | |        MULT Signals       | |   FPU Signals   | |  Reg  | |  CSR Signals  | |           LSU Signals            |    atop  |       Jump relative Signals      |
    val default: List[UInt] = List(N,N, N,N,N,N,N,N, N, N, N, N,N,N,N,N,N,N,  Y, SLTU, OP_A_REGA_OR_FWD, OP_B_REGB_OR_FWD, OP_C_REGC_OR_FWD, IMMA_ZERO, IMMB_I, REGC_ZERO,  I,N, MIMM_ZERO, N, 0.U(2.W),  FP32, FP32, INT32,  N,N,N,Y,  N,N,CSR_OP_READ,  N,N,Y,0.U(2.W),0.U(2.W),0.U(2.W),N,  0.U(6.W),  BRANCH_NONE, BRANCH_NONE, JT_JAL)
}

class Decoder()(p: IDParams) extends Module with RequireAsyncReset {
    val io = IO(new DecoderIO()(p))

    /** Internal signals */
    //write enable/request signals
    val regfileMemWe        = Wire(Bool())
    val regfileAluWe        = Wire(Bool())
    val dataReq             = Wire(Bool())
    val csrIllegal          = Wire(Bool())
    val ctrlTransferInsn    = Wire(UInt(2.W))

    val csrOp               = Wire(UInt(CSROp.width.W))

    val aluEn               = Wire(Bool())
    val multIntEn           = Wire(Bool())
    val multDotEn           = Wire(Bool())
    val apuEn               = Wire(Bool())


    val checkFprm           = Wire(Bool())
    val fpuOp               = Wire(UInt(FPU.OP_BITS.W))     // FPU operation
    val fpuOpMod            = Wire(Bool())                  // FPU operation modifier
    val fpuVecOp            = Wire(Bool())                  // FPU vectorial operation

    // unittypes for latencies to help us decode for APU
    val fpOpGroup           = Wire(UInt(2.W))

    //////////////
    //////////////
    /////Decoder
    //////////////
    //////////////

    ///////////////////
    //// wire init
    //////////////////
    ctrlTransferInsn                := BRANCH_NONE
    io.ctrlTransferTargetMuxSel     := JT_JAL

    aluEn                       := true.B
    io.ctrlSig.aluSig.en        := SLTU
    io.ctrlSig.aluSig.opAMuxSel := OP_A_REGA_OR_FWD
    io.ctrlSig.aluSig.opBMuxSel := OP_B_REGB_OR_FWD
    io.ctrlSig.aluSig.opCMuxSel := OP_C_REGC_OR_FWD
    io.ctrlSig.aluSig.immAMuxSel:= IMMA_ZERO
    io.ctrlSig.aluSig.immBMuxSel:= IMMB_I
    io.ctrlSig.aluSig.regCMux   := REGC_ZERO


    switch(io.insnRdata(6,0)){


        //////////////////////
        /////JUMPS
        //////////////////////
        is(JAL){//Jump and Link
            io.ctrlTransferTargetMuxSel := JT_JAL
            ctrlTransferInsn            := BRANCH_JAL
            //Calculate and store PC+4
            io.ctrlSig.aluSig.opAMuxSel := OP_A_CURRPC
            io.ctrlSig.aluSig.opBMuxSel := OP_B_IMM
            io.ctrlSig.aluSig.immBMuxSel:= IMMB_PCINCR
            io.ctrlSig.aluSig.operator  := ALUOp.ADD
            regfileAluWe                := true.B
            // Calculate jump target (= PC +UJ imm)
        } //is(JAL)

        is(JALR){//Jump and Link Register
            when(io.insnRdata(14,12) === 0.U){
                io.ctrlTransferTargetMuxSel := JT_JALR
                ctrlTransferInsn            := BRANCH_JALR
                //Calculate and store PC+4
                io.ctrlSig.aluSig.opAMuxSel := OP_A_CURRPC
                io.ctrlSig.aluSig.opBMuxSel := OP_B_IMM
                io.ctrlSig.aluSig.immBMuxSel:= IMMB_PCINCR
                io.ctrlSig.aluSig.operator  := ALUOp.ADD
                regfileAluWe                := true.B
                // Calculate jump target (= RS1 + I imm)
                io.regUsed.a                := true.B
            }.otherwise{
                ctrlTransferInsn            := BRANCH_NONE
                regfileAluWe                := false.B
                io.illegalInsn              := true.B
            }
        }//is(JALR)

        is(BRANCH){//Branch
            io.ctrlTransferTargetMuxSel := JT_COND
            ctrlTransferInsn            := BRANCH_COND
            io.ctrlSig.aluSig.opCMuxSel := OP_C_JT
            io.regUsed.a                := true.B
            io.regUsed.b                := true.B
            switch(io.insnRdata(14,12)){
                is(0.U(3.W)){io.ctrlSig.aluSig.operator := EQ}
                is(1.U(3.W)){io.ctrlSig.aluSig.operator := NE}
                is(4.U(3.W)){io.ctrlSig.aluSig.operator := LTS}
                is(5.U(3.W)){io.ctrlSig.aluSig.operator := GES}
                is(6.U(3.W)){io.ctrlSig.aluSig.operator := LTU}
                is(7.U(3.W)){io.ctrlSig.aluSig.operator := GEU}
                is(2.U(3.W)){io.illegalInsn := true.B}
                is(3.U(3.W)){io.illegalInsn := true.B}
            }
        }//is(BRANCH)


        /////////////////////////
        //////LDST
        /////////////////////////
        is(STORE) {
            dataReq                         := true.B
            io.ctrlSig.lsuSig.dataWe        := true.B
            io.regUsed.a                    := true.B
            io.regUsed.b                    := true.B
            io.ctrlSig.aluSig.operator      := ALUOp.ADD
            //pass write data through ALU operand c
            io.ctrlSig.aluSig.opCMuxSel     := OP_C_REGB_OR_FWD

            when(io.insnRdata(14) === false.B){
                //offset from immediate
                io.ctrlSig.aluSig.immBMuxSel        := IMMB_S
                io.ctrlSig.aluSig.opBMuxSel         := OP_B_IMM
            }.otherwise{
                io.illegalInsn      := true.B
            }
            // store size
            switch(io.insnRdata(13,12)){
                is(0.U){ io.ctrlSig.lsuSig.dataType := 2.U } //SB
                is(1.U){ io.ctrlSig.lsuSig.dataType := 1.U } //SH
                is(2.U){ io.ctrlSig.lsuSig.dataType := 0.U } //SW
                is(3.U){ 
                    dataReq                     := false.B
                    io.ctrlSig.lsuSig.dataWe    := false.B
                    io.illegalInsn              := true.B
                 }
            }
        }//is(STORE)

        is(LOAD){
            dataReq                         := true.B
            regfileMemWe                    := true.B
            io.regUsed.a                    := true.B
            io.ctrlSig.lsuSig.dataType      := 0.U(2.W)
            //offset from immediate
            io.ctrlSig.aluSig.operator      := ALUOp.ADD
            io.ctrlSig.aluSig.opBMuxSel     := OP_B_IMM
            io.ctrlSig.aluSig.immBMuxSel    := IMMB_I

            //sign/zero extension
            io.ctrlSig.lsuSig.dataSignExtension := Cat( 0.U(1.W) , ~io.insnRdata(14))

            // load size
            io.ctrlSig.lsuSig.dataType  := 
                MuxLookup( io.insnRdata(13,12) , 
                0.U(2.W) , Array(0.U -> 2.U , 1.U -> 1.U , 2.U -> 0.U)
                // illegal or reg-reg   LB        LH           LW
            )
        }//is(LOAD)

        is(AMO){
             io.illegalInsn              := true.B
        }//is(AMO)


        //////////////////////////
        //////ALU
        //////////////////////////
        is(LUI){// Load Upper Immediate
            io.ctrlSig.aluSig.opAMuxSel     := OP_A_IMM
            io.ctrlSig.aluSig.opBMuxSel     := OP_B_IMM
            io.ctrlSig.aluSig.immAMuxSel    := IMMA_ZERO
            io.ctrlSig.aluSig.immBMuxSel    := IMMB_U
            io.ctrlSig.aluSig.operator      := ALUOp.ADD
            regfileAluWe                    := true.B
        }//is(LUI)

        is(AUIPC){//Add upper Immediate to PC
            io.ctrlSig.aluSig.opAMuxSel     := OP_A_CURRPC
            io.ctrlSig.aluSig.opBMuxSel     := OP_B_IMM
            io.ctrlSig.aluSig.immBMuxSel    := IMMB_U
            io.ctrlSig.aluSig.operator      := ALUOp.ADD
            regfileAluWe                    := true.B
        }//is(AUIPC)

        is(OPIMM){//Register_Immediate ALU OPerations
            io.ctrlSig.aluSig.opBMuxSel     := OP_B_IMM
            io.ctrlSig.aluSig.immBMuxSel    := IMMB_I
            regfileAluWe                    := true.B
            io.regUsed.a                    := true.B
            switch(io.insnRdata(14,12)){
                is(0.U(3.W)){ io.ctrlSig.aluSig.operator := ALUOp.ADD } //add imm
                is(2.U(3.W)){ io.ctrlSig.aluSig.operator := ALUOp.SLTS }//set to one if lower than imm
                is(3.U(3.W)){ io.ctrlSig.aluSig.operator := ALUOp.SLTU }//set to one if lower than imm
                is(4.U(3.W)){ io.ctrlSig.aluSig.operator := ALUOp.XOR }// exclusive or with imm
                is(6.U(3.W)){ io.ctrlSig.aluSig.operator := ALUOp.OR }// or with imm
                is(7.U(3.W)){ io.ctrlSig.aluSig.operator := ALUOp.AND }//and with imm
                is(1.U(3.W)){ 
                    io.ctrlSig.aluSig.operator := ALUOp.SLL //shift left logical imm
                    when(io.insnRdata(31,25) =/= 0.U(7.W)){
                        io.illegalInsn      := true.B
                    }
                }
                is(5.U(3.W)){ 
                    when(io.insnRdata(31,25) === 0.U(7.W)){
                        io.ctrlSig.aluSig.operator := ALUOp.SRL //shift right logical by imm
                    }.elsewhen(io.insnRdata(31,25) === 32.U(7.W)){
                        io.ctrlSig.aluSig.operator := ALUOp.SRA //shift right rithmetically by imm
                    }.otherwise{
                        io.illegalInsn   := true.B
                    }
                }
            }//switch(io.insnRdata(14,12))
        }//is(OPIMM)

        is(OP){//Reg-Reg ALU operation
            when(io.insnRdata(31,30) === 3.U){//PREFIX 11
                io.illegalInsn  := true.B
            }.elsewhen(io.insnRdata(31,30) === 2.U){//PREFIX 10
                when(io.insnRdata(29,25) === 0.U){//REG BIT-MANIPULATION
                    io.illegalInsn  := true.B
                }.otherwise{//VECTORIAL FLOAT OPS

                //???????????????????????????????????? 
                //????????????????????????????????????
                    when( (FPU == 1).asBool() ){// Vectorial FP not available in 'old' shared FPU
                    }.otherwise{// FPU!=1 or no vectors or old shared unit
                        io.illegalInsn  := true.B
                    }
                }
            }.otherwise{//PREFIX 01/00
                // non bit-manipulation instructions
                regfileAluWe    := true.B
                io.regUsed.a    := true.B
                //??? like this , Is there latch???
                when(~io.insnRdata(28)){
                    io.regUsed.b := true.B
                }

                switch( Cat( io.insnRdata(30,25) , io.insnRdata(14,12) )){
                    //RV32I ALU operations
                    is("b000000000".U){ io.ctrlSig.aluSig.operator := ALUOp.ADD} //add
                    is("b100000000".U){ io.ctrlSig.aluSig.operator := ALUOp.SUB} //sub
                    is("b000000010".U){ io.ctrlSig.aluSig.operator := ALUOp.SLTS} //set lower than
                    is("b000000011".U){ io.ctrlSig.aluSig.operator := ALUOp.SLTU} //set lower than unsigned
                    is("b000000100".U){ io.ctrlSig.aluSig.operator := ALUOp.XOR} //xor
                    is("b000000110".U){ io.ctrlSig.aluSig.operator := ALUOp.OR}  //or
                    is("b000000111".U){ io.ctrlSig.aluSig.operator := ALUOp.AND} //and
                    is("b000000001".U){ io.ctrlSig.aluSig.operator := ALUOp.SLL} //shift left logical
                    is("b000000101".U){ io.ctrlSig.aluSig.operator := ALUOp.SRL} //shift right logical
                    is("b100000101".U){ io.ctrlSig.aluSig.operator := ALUOp.SRA} //shift right Arithmetic

                    //supported RV32M instructions
                    is("b000001000".U){ //mul
                        aluEn       := false.B
                        multIntEn   := true.B
                        io.ctrlSig.mulSig.operator      := MAC32
                        io.ctrlSig.aluSig.regCMux       := REGC_ZERO
                    }
                    is("b000001001".U){ //mulh
                        aluEn                           := false.B
                        io.regUsed.c                    := true.B
                        io.ctrlSig.aluSig.regCMux       := REGC_ZERO
                        io.ctrlSig.mulSig.signedMode    := 3.U(2.W)
                        io.ctrlSig.mulSig.intEn         := true.B
                        io.ctrlSig.mulSig.operator      := H        
                    }
                    is("b000001010".U){ //mulhsu
                        aluEn                           := false.B
                        io.regUsed.c                    := true.B
                        io.ctrlSig.aluSig.regCMux       := REGC_ZERO
                        io.ctrlSig.mulSig.signedMode    := 1.U(2.W)
                        io.ctrlSig.mulSig.intEn         := true.B
                        io.ctrlSig.mulSig.operator      := H  
                    }
                    is("b000001011".U){ //mulhu
                        aluEn                           := false.B
                        io.regUsed.c                    := true.B
                        io.ctrlSig.aluSig.regCMux       := REGC_ZERO
                        io.ctrlSig.mulSig.signedMode    := 0.U(2.W)
                        io.ctrlSig.mulSig.intEn         := true.B
                        io.ctrlSig.mulSig.operator      := H  
                    }
                    is("b000001100".U){ //mul
                        io.ctrlSig.aluSig.opAMuxSel     := OP_A_REGB_OR_FWD
                        io.ctrlSig.aluSig.opBMuxSel     := OP_B_REGA_OR_FWD
                        io.regUsed.b                    := true.B
                        io.ctrlSig.aluSig.operator      := ALUOp.DIV
                    }
                    is("b000001101".U){ //mul
                        io.ctrlSig.aluSig.opAMuxSel     := OP_A_REGB_OR_FWD
                        io.ctrlSig.aluSig.opBMuxSel     := OP_B_REGA_OR_FWD
                        io.regUsed.b                    := true.B
                        io.ctrlSig.aluSig.operator      := ALUOp.DIVU
                    }
                    is("b000001110".U){ //mul
                        io.ctrlSig.aluSig.opAMuxSel     := OP_A_REGB_OR_FWD
                        io.ctrlSig.aluSig.opBMuxSel     := OP_B_REGA_OR_FWD
                        io.regUsed.b                    := true.B
                        io.ctrlSig.aluSig.operator      := ALUOp.REM
                    }
                    is("b000001111".U){ //remu
                        io.ctrlSig.aluSig.opAMuxSel     := OP_A_REGB_OR_FWD
                        io.ctrlSig.aluSig.opBMuxSel     := OP_B_REGA_OR_FWD
                        io.regUsed.b                    := true.B
                        io.ctrlSig.aluSig.operator      := ALUOp.REMU
                    }
                }

            }
        }//is(OP)


        ///////////////////////////
        //////FPU
        ///////////////////////////
        is(OP_FP){
            when((p.FPU == 1).asBool()){

                // using APU instead of ALU
                apuEn           := true.B
                aluEn           := false.B
                // by default, set all registers to FP registers use 2
                io.regUsed.a    := true.B
                io.regUsed.b    := true.B
                io.regUsed.aFP  := true.B
                io.regUsed.bFP  := true.B
                io.regUsed.dFP  := true.B
                // by default we need to verify rm is legal but asume it is for now
                checkFprm       := true.B
                io.ctrlSig.apuSig.fpRndMode := io.insnRdata(14,12)
                
                //Decode Formats(preliminary , can change for some ops)
                switch(io.insnRdata(26,25)){
                    is(0.U){
                        io.ctrlSig.fpuSig.dstFmt := FP32
                    }
                    is(1.U){
                        io.ctrlSig.fpuSig.dstFmt := FP64
                    }
                    is(2.U){
                        when(io.insnRdata(14,12) === 5.U){
                            io.ctrlSig.fpuSig.dstFmt := FP16ALT
                        }.otherwise{
                            io.ctrlSig.fpuSig.dstFmt := FP16
                        }
                    }
                    is(3.U){
                        io.ctrlSig.fpuSig.dstFmt := FP8
                    }
                }

                // By default , src = dst
                io.ctrlSig.fpuSig.srcFmt := io.ctrlSig.fpuSig.dstFmt
                
                // decode FP instruction
                when( io.insnRdata(31,27) === "b00000".U ){ 
                    // fadd.fmt - FP Addition
                    fpuOp                           := FPU.ADD
                    fpOpGroup                       := ADDMUL
                    io.ctrlSig.apuSig.op            := 0.U(2.W)
                    io.ctrlSig.apuSig.lat           := Mux((APU.PIPE_REG_ADDSUB == 1).asBool(), 2.U , 1.U)
                    io.ctrlSig.aluSig.opBMuxSel     := OP_B_REGA_OR_FWD
                    io.ctrlSig.aluSig.opCMuxSel     := OP_C_REGB_OR_FWD
                }.elsewhen( io.insnRdata(31,27) === "b00001".U ){
                    // fsub.fmt -FP Subtraction
                    fpuOp                           := FPU.ADD
                    fpuOpMod                        := 1.U
                    fpOpGroup                       := ADDMUL
                    io.ctrlSig.apuSig.op            := 1.U(2.W)
                    io.ctrlSig.apuSig.lat           := Mux((APU.PIPE_REG_ADDSUB == 1).asBool(), 2.U , 1.U)
                    io.ctrlSig.aluSig.opBMuxSel     := OP_B_REGA_OR_FWD
                    io.ctrlSig.aluSig.opCMuxSel     := OP_C_REGB_OR_FWD                    
                }.elsewhen( io.insnRdata(31,27) === "b00010".U ){
                    // fmul.fmt - FP Multiplication
                    fpuOp                           := FPU.MUL
                    fpOpGroup                       := ADDMUL
                    io.ctrlSig.apuSig.lat           := Mux((APU.PIPE_REG_MULT == 1).asBool(), 2.U , 1.U)                  
                }.elsewhen( io.insnRdata(31,27) === "b00011".U ){
                    // fdiv.fmt -FP Division
                    fpuOp                           := FPU.DIV
                    fpOpGroup                       := DIVSQRT
                    io.ctrlSig.apuSig.lat           := 3.U        
                }.elsewhen( io.insnRdata(31,27) === "b01011".U ){
                    // fsqrt.fmt - FP Square Root
                    io.regUsed.b                    := 0.U
                    fpuOp                           := FPU.SQRT
                    fpOpGroup                       := DIVSQRT
                    io.ctrlSig.apuSig.op            := 1.U
                    io.ctrlSig.apuSig.lat           := 3.U 
                    // rs2 must be zero
                    when( io.insnRdata(24,20) =/= 0.U){
                        io.illegalInsn := true.B
                    }.otherwise{
                        io.illegalInsn := false.B
                    }
                }.elsewhen( io.insnRdata(31,27) === "b00100".U ){
                    // fsgn{j[n]/jx}.fmt - FP Sign Injection
                    fpuOp               := FPU.SGNJ
                    fpOpGroup           := NONCOMP
                    checkFprm           := false.B   // instruction encoded in rm, do the check here
                    when(C_XF16ALT){// FP16ALT instrutions encoded in rm separately (static)
                        when( io.insnRdata(14,12) === 3.U || io.insnRdata(14,12) === 7.U){
                            io.illegalInsn      := true.B
                        }
                        // FP16ALT uses special encoding here
                        when( io.insnRdata(14) ){
                            io.ctrlSig.fpuSig.dstFmt    := FP16ALT
                            io.ctrlSig.fpuSig.srcFmt    := FP16ALT
                        }.otherwise{
                            io.ctrlSig.apuSig.fpRndMode := Cat(0.U(1.W),io.insnRdata(13,12))
                        }           
                    }.otherwise{
                        io.illegalInsn      := true.B
                    }
                }.elsewhen( io.insnRdata(31,27) === "b00101".U ){
                    // fmin/fmax.fmt -FP Minimum/Maximum
                    fpuOp                           := MINMAX
                    fpOpGroup                       := NONCOMP
                    checkFprm           := false.B   // instruction encoded in rm, do the check here
                    when(C_XF16ALT){// FP16ALT instrutions encoded in rm separately (static)
                        when( io.insnRdata(14,12) === 3.U || io.insnRdata(14,12) === 2.U
                            || io.insnRdata(14,12) === 7.U || io.insnRdata(14,12) === 6.U){
                            io.illegalInsn      := true.B
                        }
                        // FP16ALT uses special encoding here
                        when( io.insnRdata(14) ){
                            io.ctrlSig.fpuSig.dstFmt    := FP16ALT
                            io.ctrlSig.fpuSig.srcFmt    := FP16ALT
                        }.otherwise{
                            io.ctrlSig.apuSig.fpRndMode := Cat(0.U(1.W),io.insnRdata(13,12))
                        }           
                    }.otherwise{
                        io.illegalInsn      := true.B
                    }
                }.elsewhen( io.insnRdata(31,27) === "b01000".U ){
                    // fcvt.fmt.fmt -FP to FP Conversion
                    io.regUsed.b                    := false.B
                    fpuOp                           := F2F
                    fpOpGroup                       := CONV
                    
                    //bits[22,20] used, other bits must be 0
                    when(io.insnRdata(24,23) === 0.U){//check source format
                        when(io.insnRdata(22,20) === 0.U){
                            when(C_RVF){
                                io.ctrlSig.fpuSig.srcFmt := FP32
                            }.otherwise{
                                io.illegalInsn      := true.B
                            }
                        }.elsewhen(io.insnRdata(22,20) === 1.U){
                            when(C_RVD){
                                io.ctrlSig.fpuSig.srcFmt := FP64
                            }.otherwise{
                                io.illegalInsn      := true.B
                            }
                        }.elsewhen(io.insnRdata(22,20) === 2.U){
                            when(C_XF16){
                                io.ctrlSig.fpuSig.srcFmt := FP16
                            }.otherwise{
                                io.illegalInsn      := true.B
                            }
                        }.elsewhen(io.insnRdata(22,20) === 6.U){
                            when(C_XF16ALT){
                                io.ctrlSig.fpuSig.srcFmt := FP16ALT
                            }.otherwise{
                                io.illegalInsn      := true.B
                            }
                        }.elsewhen(io.insnRdata(22,20) === 3.U){
                            when(C_XF8){
                                io.ctrlSig.fpuSig.srcFmt := FP8
                            }.otherwise{
                                io.illegalInsn      := true.B
                            }
                        }.otherwise{
                            io.illegalInsn := true.B
                        }
    
                    }.otherwise{
                        io.illegalInsn := true.B
                    }
                }.elsewhen( io.insnRdata(31,27) === "b01001".U ){
                    // fmulex.s.fmt -FP Expanding Multiplicaion to FP32
                    fpuOp                           := FPU.MUL
                    fpOpGroup                       := ADDMUL
                    io.ctrlSig.apuSig.lat           := Mux((APU.PIPE_REG_MULT == 1).asBool(), 2.U , 1.U)
                    // set dst format to FP32
                    io.ctrlSig.fpuSig.dstFmt        := FP32                 
                }.elsewhen( io.insnRdata(31,27) === "b01010".U ){
                    // fmacex.s.fmt -FP Expanding Multiplicaion-Accumulate to FP32
                    io.regUsed.c                    := true.B
                    io.ctrlSig.aluSig.regCMux       := REGC_RD // third operand is rd
                    io.regUsed.cFP                  := true.B
                    fpuOp                           := FPU.FMADD
                    fpOpGroup                       := ADDMUL
                    io.ctrlSig.apuSig.lat           := Mux((APU.PIPE_REG_MULT == 1).asBool(), 2.U , 1.U)
                    // set dst format to FP32
                    io.ctrlSig.fpuSig.dstFmt        := FP32                 
                }.elsewhen( io.insnRdata(31,27) === "b10100".U ){
                    // feq/flt/fle.fmt -FP Comparisons
                    fpuOp                           := FPU.CMP
                    fpOpGroup                       := NONCOMP
                    io.regUsed.dFP                  := false.B
                    checkFprm                       := false.B
                    when(C_XF16ALT){// FP16ALT instrutions encoded in rm separately (static)
                        when( io.insnRdata(14,12) === 3.U || io.insnRdata(14,12) === 7.U){
                            io.illegalInsn      := true.B
                        }
                        // FP16ALT uses special encoding here
                        when( io.insnRdata(14) ){
                            io.ctrlSig.fpuSig.dstFmt    := FP16ALT
                            io.ctrlSig.fpuSig.srcFmt    := FP16ALT
                        }.otherwise{
                            io.ctrlSig.apuSig.fpRndMode := Cat(0.U(1.W),io.insnRdata(13,12))
                        }           
                    }.otherwise{
                        io.illegalInsn      := true.B
                    }
                }.elsewhen( io.insnRdata(31,27) === "b11000".U ){
                    // fcvt.ifmt.fmt -FP to Int Conversion
                    io.regUsed.b                    := false.B
                    io.regUsed.dFP                  := false.B //go to integer regfile
                    fpuOp                           := F2I
                    fpOpGroup                       := CONV
                    fpuOpMod                        := io.insnRdata(20) // signed/unsigned switch
                    io.ctrlSig.apuSig.op            := 1.U
                    io.ctrlSig.apuSig.lat           := Mux((PIPE_REG_CAST == 1).asBool(), 2.U, 1.U)
                    
                    when( io.insnRdata(24,21) === 0.U ){
                        switch( io.insnRdata(26,25) ){ //fix for 
                            is(0.U){
                                when(C_RVF){
                                    io.ctrlSig.fpuSig.srcFmt := FP32
                                }.otherwise{
                                    io.illegalInsn      := true.B
                                }
                            }
                            is(1.U){
                                when(C_RVD){
                                    io.ctrlSig.fpuSig.srcFmt := FP64
                                }.otherwise{
                                    io.illegalInsn      := true.B
                                }
                            }
                            is(2.U){
                                when( io.insnRdata(14,12) === 5.U ){
                                    when(C_XF16ALT){
                                        io.ctrlSig.fpuSig.srcFmt := FP16ALT
                                    }.otherwise{
                                        io.illegalInsn      := true.B
                                    }
                                }.elsewhen(~C_XF16){
                                    io.illegalInsn      := true.B
                                }.otherwise{
                                    io.ctrlSig.fpuSig.srcFmt    := FP16
                                }
                            }
                            is(3.U){
                                when(C_XF8){
                                    io.ctrlSig.fpuSig.srcFmt := FP8
                                }.otherwise{
                                    io.illegalInsn      := true.B
                                }
                            }
                        }
                    }.otherwise{
                        io.illegalInsn := true.B
                    }
                }.elsewhen( io.insnRdata(31,27) === "b11010".U ){
                    // fcvt.fmt.ifmt - Int to FP Conversion
                    io.regUsed.b                    := false.B
                    io.regUsed.aFP                  := false.B
                    fpuOp                           := I2F
                    fpOpGroup                       := CONV
                    fpuOpMod                        := io.insnRdata(20)
                    io.ctrlSig.apuSig.op            := 0.U
                    io.ctrlSig.apuSig.lat           := Mux((PIPE_REG_CAST == 1).asBool(), 2.U, 1.U ) 
                    // bits [21:20] used, other bits must be 0
                    when( io.insnRdata(24,21) =/= 0.U){ // in RV32, no casts to L allowed.
                        io.illegalInsn := true.B
                    }.otherwise{
                        io.illegalInsn := false.B
                    }
                }.elsewhen( io.insnRdata(31,27) === "b11100".U ){
                    // move and class
                    io.regUsed.b                    := false.B
                    io.regUsed.dFP                  := false.B // go to integer regfile
                    fpOpGroup                       := NONCOMP
                    checkFprm                       := false.B
                    // fmv.x.fmt - FPR to GPR Move
                    when( io.insnRdata(14,12) === 0.U || ( C_XF16ALT && (io.insnRdata(14,12) === 4.U )) ){
                        io.ctrlSig.aluSig.opBMuxSel := OP_B_REGA_OR_FWD
                        fpuOp                       := SGNJ
                        fpuOpMod                    := true.B
                        io.ctrlSig.apuSig.fpRndMode := 3.U
                        when(io.insnRdata(14)){
                            io.ctrlSig.fpuSig.dstFmt    := FP16ALT
                            io.ctrlSig.fpuSig.srcFmt    := FP16ALT
                        }
                    // fclass.fmt -FP Classify
                    }.elsewhen( io.insnRdata(14,12) ===1.U || ( C_XF16ALT && (io.insnRdata(14,12) === 5.U ))  ){
                        fpuOp                       := CLASSIFY
                        io.ctrlSig.apuSig.fpRndMode := 0.U
                        when( io.insnRdata(14) ){
                            io.ctrlSig.fpuSig.dstFmt    := FP16ALT
                            io.ctrlSig.fpuSig.srcFmt    := FP16ALT
                        }

                    }.otherwise{
                        io.illegalInsn := true.B
                    }
                    when(io.insnRdata(24,20) =/= 0.U){
                        io.illegalInsn := true.B
                    }.otherwise{
                        io.illegalInsn := false.B
                    }
                }.elsewhen( io.insnRdata(31,27) === "b11110".U ){
                    // fmv.fmt.x - GPR to FPR move
                    io.regUsed.b                    := false.B
                    io.regUsed.aFP                  := false.B
                    io.ctrlSig.aluSig.opBMuxSel     := OP_B_REGA_OR_FWD
                    fpuOp                           := SGNJ
                    fpOpGroup                       := NONCOMP
                    fpuOpMod                        := false.B
                    io.ctrlSig.apuSig.fpRndMode     := 3.U
                    checkFprm                       := false.B
                    when( io.insnRdata(14,12) === 0.U || (C_XF16ALT && (io.insnRdata(14,12) === 4.U )) ){
                        when( io.insnRdata(14) )
                        {
                            io.ctrlSig.fpuSig.dstFmt    := FP16ALT
                            io.ctrlSig.fpuSig.srcFmt    := FP16ALT
                        }
                    }.otherwise{
                        io.illegalInsn := true.B
                    }
                    when( io.insnRdata(24,20) =/= 0.U ){
                        io.illegalInsn := true.B
                    }.otherwise{
                        io.illegalInsn := false.B
                    }
                }.otherwise{
                    // Rest are illegal instructions
                    io.illegalInsn := true.B
                }

                // check enable formats (static)
                when( ~C_RVF && io.ctrlSig.fpuSig.dstFmt ===FP32 ){
                    io.illegalInsn  := true.B
                }.otherwise{
                    io.illegalInsn  := false.B
                }
                when( ~C_RVD && io.ctrlSig.fpuSig.dstFmt ===FP64 ){
                    io.illegalInsn  := true.B
                }.otherwise{
                    io.illegalInsn  := false.B
                }
                when( ~C_XF16 && io.ctrlSig.fpuSig.dstFmt ===FP16 ){
                    io.illegalInsn  := true.B
                }.otherwise{
                    io.illegalInsn  := false.B
                }
                when( ~C_XF16ALT && io.ctrlSig.fpuSig.dstFmt ===FP16ALT ){
                    io.illegalInsn  := true.B
                }.otherwise{
                    io.illegalInsn  := false.B
                }
                when( ~C_XF8 && io.ctrlSig.fpuSig.dstFmt ===FP8 ){
                    io.illegalInsn  := true.B
                }.otherwise{
                    io.illegalInsn  := false.B
                }

                // check rounding mode
                when(checkFprm){
                    when(io.insnRdata(14,12) === 0.U || io.insnRdata(14,12) === 1.U 
                    || io.insnRdata(14,12) === 2.U || io.insnRdata(14,12) === 3.U
                    || io.insnRdata(14,12) === 4.U){

                    }.elsewhen( io.insnRdata(14,12) === 5.U ){
                        when( ~C_XF16ALT || io.ctrlSig.fpuSig.dstFmt =/= FP16ALT){
                            io.illegalInsn  := true.B
                        }.otherwise{
                            when(io.ctrlSig.fpuSig.frm === 0.U || io.ctrlSig.fpuSig.frm === 1.U 
                            || io.ctrlSig.fpuSig.frm === 2.U || io.ctrlSig.fpuSig.frm === 3.U
                            || io.ctrlSig.fpuSig.frm === 4.U){
                                io.ctrlSig.apuSig.fpRndMode     := io.ctrlSig.fpuSig.frm
                            }otherwise{
                                io.illegalInsn  := true.B
                            }
                        }
                    }.elsewhen( io.insnRdata(14,12) === 7.U ){
                        when(io.ctrlSig.fpuSig.frm === 0.U || io.ctrlSig.fpuSig.frm === 1.U 
                            || io.ctrlSig.fpuSig.frm === 2.U || io.ctrlSig.fpuSig.frm === 3.U
                            || io.ctrlSig.fpuSig.frm === 4.U){
                                io.ctrlSig.apuSig.fpRndMode     := io.ctrlSig.fpuSig.frm
                            }otherwise{
                                io.illegalInsn  := true.B
                            }
                    }.otherwise{
                        io.illegalInsn  := true.B
                    }
                }

                // Set latencies for FPnew feom config. The C_LAT contain the number
                // of pipeline registers. the APU takes the following values:
                // 1 = single (no latency , 2 = one pipeline, 3 = two or more pipestages)
                    switch(fpOpGroup){
                        is(ADDMUL){
                            when(io.ctrlSig.fpuSig.dstFmt === FP32){
                                io.ctrlSig.apuSig.lat   := Mux(C_LAT_FP32 < 2.U , C_LAT_FP32 + 1.U , 3.U)
                            }.elsewhen(io.ctrlSig.fpuSig.dstFmt === FP64){
                                io.ctrlSig.apuSig.lat   := Mux(C_LAT_FP64 < 2.U , C_LAT_FP64 + 1.U , 3.U)
                            }.elsewhen(io.ctrlSig.fpuSig.dstFmt === FP16){
                                io.ctrlSig.apuSig.lat   := Mux(C_LAT_FP16 < 2.U , C_LAT_FP16 + 1.U , 3.U)
                            }.elsewhen(io.ctrlSig.fpuSig.dstFmt === FP16ALT){
                                io.ctrlSig.apuSig.lat   := Mux(C_LAT_FP16ALT < 2.U , C_LAT_FP16ALT + 1.U , 3.U)
                            }.elsewhen(io.ctrlSig.fpuSig.dstFmt === FP8){
                                io.ctrlSig.apuSig.lat   := Mux(C_LAT_FP8 < 2.U , C_LAT_FP8 + 1.U , 3.U)
                            }.otherwise{

                            }
                        }
                        is(DIVSQRT){
                            io.ctrlSig.apuSig.lat       := 3.U
                        }
                        is(NONCOMP){
                            io.ctrlSig.apuSig.lat   := Mux(C_LAT_NONCOMP < 2.U , C_LAT_NONCOMP + 1.U , 3.U)
                        }
                        is(CONV){
                            io.ctrlSig.apuSig.lat   := Mux(C_LAT_CONV < 2.U , C_LAT_CONV + 1.U , 3.U)
                        }
                    }
                // Set FPnew Op and OPMOD as the APU op
                    io.ctrlSig.apuSig.op        := Cat(fpuVecOp,fpuOpMod,fpuOp)

            }.otherwise{
                io.illegalInsn := true.B
            }
        }//is(OP_FP)

        // floating point fused arithmetic
        is(OP_FMADD ,OP_FMSUB,OP_FNMSUB,OP_FNMADD){
            when((p.FPU == 1).asBool()){
                // using APU instead of ALU
                apuEn                       := true.B
                aluEn                       := false.B
                io.ctrlSig.apuSig.lat       := Mux((PIPE_REG_MAC > 1).asBool(), 3.U, 2.U )
                io.regUsed.a                := true.B
                io.regUsed.b                := true.B
                io.regUsed.c                := true.B
                io.ctrlSig.aluSig.opCMuxSel := REGC_S4
                io.regUsed.aFP              := true.B
                io.regUsed.bFP              := true.B
                io.regUsed.cFP              := true.B
                io.regUsed.dFP              := true.B
                io.ctrlSig.apuSig.fpRndMode := io.insnRdata(14,12)

                // Decode Formats
                switch( io.insnRdata(26,25) ){
                    is(0.U){
                        io.ctrlSig.fpuSig.dstFmt    := FP32
                    }
                    is(1.U){
                        io.ctrlSig.fpuSig.dstFmt    := FP64
                    }
                    is(2.U){
                        when( io.insnRdata(14,12) === 5.U){
                            io.ctrlSig.fpuSig.dstFmt    := FP16ALT
                        }.otherwise{
                            io.ctrlSig.fpuSig.dstFmt    := FP16
                        }
                    }
                    is(3.U){
                        io.ctrlSig.fpuSig.dstFmt    := FP8
                    }
                }

                // By default ,src = dst
                io.ctrlSig.fpuSig.srcFmt := io.ctrlSig.fpuSig.dstFmt

                // decode FP instruction
                switch( io.insnRdata(6,0) ){
                    // fmadd.fmt - FP Fused multiply-add
                    is( OP_FMADD ){
                        fpuOp                   := FPU.FMADD
                        io.ctrlSig.apuSig.op    := 0.U
                    }
                    // fmsub.fmt -FP Fused multiply-subtract
                    is( OP_FMSUB ){
                        fpuOp                   := FPU.FMADD
                        fpuOpMod                := true.B
                        io.ctrlSig.apuSig.op    := 1.U
                    }
                    // fnmsub.fmt -FP Negated fused multiply-subtract
                    is( OP_FNMSUB ){
                        fpuOp                   := FPU.FNMSUB
                        io.ctrlSig.apuSig.op    := 2.U
                    }
                    // fnmsub.fmt -FP Negated fused multiply-add
                    is( OP_FNMADD ){
                        fpuOp                   := FPU.FNMSUB
                        fpuOpMod                := true.B
                        io.ctrlSig.apuSig.op    := 3.U
                    }
                }

                // check enabled formats (static)
                when(~C_RVF && (io.ctrlSig.fpuSig.dstFmt === FP32)){
                    io.illegalInsn      := true.B
                }.otherwise{
                    io.illegalInsn      := false.B
                }
                when(~C_RVD && (io.ctrlSig.fpuSig.dstFmt === FP64)){
                    io.illegalInsn      := true.B
                }.otherwise{
                    io.illegalInsn      := false.B
                }
                when(~C_XF16 && (io.ctrlSig.fpuSig.dstFmt === FP16)){
                    io.illegalInsn      := true.B
                }.otherwise{
                    io.illegalInsn      := false.B
                }
                when(~C_XF16ALT && (io.ctrlSig.fpuSig.dstFmt === FP16ALT)){
                    io.illegalInsn      := true.B
                }.otherwise{
                    io.illegalInsn      := false.B
                }
                when(~C_XF8 && (io.ctrlSig.fpuSig.dstFmt === FP8)){
                    io.illegalInsn      := true.B
                }.otherwise{
                    io.illegalInsn      := false.B
                }

                // check rounding mode
                when( io.insnRdata(14,12) === 0.U || io.insnRdata(14,12) === 1.U 
                      || io.insnRdata(14,12) === 2.U || io.insnRdata(14,12) === 3.U
                      || io.insnRdata(14,12) === 4.U ){
                        // legal rounding modes 
                }.elsewhen(io.insnRdata(14,12) === 5.U){
                    // Alternative half-precsision encded as fmt=10 and rm=101
                    when(~C_XF16ALT || (io.ctrlSig.fpuSig.dstFmt =/= FP16ALT)){
                        io.illegalInsn          := true.B
                    }.otherwise{
                        // actual rounding mode from frm csr
                        when(io.ctrlSig.fpuSig.frm === 0.U  || io.ctrlSig.fpuSig.frm === 0.U
                            || io.ctrlSig.fpuSig.frm === 0.U || io.ctrlSig.fpuSig.frm === 0.U
                            || io.ctrlSig.fpuSig.frm === 0.U){
                                io.ctrlSig.apuSig.fpRndMode     := io.ctrlSig.fpuSig.frm
                        }.otherwise{
                             io.illegalInsn          := true.B
                        }
                        // switch(io.ctrlSig.fpuSig.frm){
                        //     is(0.U, 1.U, 2.U, 3.U, 4.U){
                        //         io.ctrlSig.apuSig.fpRndMode     := io.ctrlSig.fpuSig.frm
                        //     }
                        //     is(5.U, 6.U, 7.U){
                        //         io.illegalInsn          := true.B
                        //     }
                        // }                
                    }
                }.elsewhen(io.insnRdata(14,12) === 7.U){
                    // roubding mode from frm csr
                    when(io.ctrlSig.fpuSig.frm === 0.U  || io.ctrlSig.fpuSig.frm === 0.U
                        || io.ctrlSig.fpuSig.frm === 0.U || io.ctrlSig.fpuSig.frm === 0.U
                        || io.ctrlSig.fpuSig.frm === 0.U){
                            io.ctrlSig.apuSig.fpRndMode     := io.ctrlSig.fpuSig.frm
                    }.otherwise{
                            io.illegalInsn          := true.B
                    }
                }.otherwise{
                    io.illegalInsn := true.B
                }

                // Set latencies for FPnew feom config. The C_LAT contain the number
                // of pipeline registers. the APU takes the following values:
                // 1 = single (no latency , 2 = one pipeline, 3 = two or more pipestages)
                // format dependent latency
                when(io.ctrlSig.fpuSig.dstFmt === FP32){
                    io.ctrlSig.apuSig.lat   := Mux(C_LAT_FP32 < 2.U , C_LAT_FP32 + 1.U , 3.U)
                }.elsewhen(io.ctrlSig.fpuSig.dstFmt === FP64){
                    io.ctrlSig.apuSig.lat   := Mux(C_LAT_FP64 < 2.U , C_LAT_FP64 + 1.U , 3.U)
                }.elsewhen(io.ctrlSig.fpuSig.dstFmt === FP16){
                    io.ctrlSig.apuSig.lat   := Mux(C_LAT_FP16 < 2.U , C_LAT_FP16 + 1.U , 3.U)
                }.elsewhen(io.ctrlSig.fpuSig.dstFmt === FP16ALT){
                    io.ctrlSig.apuSig.lat   := Mux(C_LAT_FP16ALT < 2.U , C_LAT_FP16ALT + 1.U , 3.U)
                }.elsewhen(io.ctrlSig.fpuSig.dstFmt === FP8){
                    io.ctrlSig.apuSig.lat   := Mux(C_LAT_FP8 < 2.U , C_LAT_FP8 + 1.U , 3.U)
                }.otherwise{

                }

                // Set FPnew OP and OPMOD as the APU op
                io.ctrlSig.apuSig.op        := Cat(fpuVecOp, fpuOpMod, fpuOp)
            }.otherwise{
                io.illegalInsn := true.B
            }
        } //is(OP_FMADD,OP_FMSUB,OP_FNMSUB,OP_FNMADD)

        is(STORE_FP){
            when( (p.FPU == 1).asBool() ){
                dataReq                         := true.B
                io.ctrlSig.lsuSig.dataWe        := true.B
                io.regUsed.a                    := true.B
                io.regUsed.b                    := true.B
                io.ctrlSig.aluSig.operator      := ALUOp.ADD
                io.regUsed.bFP                  := true.B

                // offset from immediate
                io.ctrlSig.aluSig.immBMuxSel    := IMMB_S
                io.ctrlSig.aluSig.opCMuxSel     := OP_B_IMM

                // pass write data through ALU operand c
                io.ctrlSig.aluSig.opCMuxSel     := OP_C_REGC_OR_FWD

                // Decode data type
                switch( io.insnRdata(14,12) ){
                    is(0.U){
                        // fsb -FP8 store
                        when(C_XF8){
                            io.ctrlSig.lsuSig.dataType  := 2.U
                        }.otherwise{
                            io.illegalInsn := true.B
                        }
                    }
                    is(1.U){
                        // fsb -FP16 store
                        when(C_XF16 | C_XF16ALT){
                            io.ctrlSig.lsuSig.dataType  := 1.U
                        }.otherwise{
                            io.illegalInsn := true.B
                        }
                    }
                    is(2.U){
                        // fsw -FP32 store
                        when(C_RVF){
                            io.ctrlSig.lsuSig.dataType  := 0.U
                        }.otherwise{
                            io.illegalInsn := true.B
                        }
                    }
                    is(3.U){
                        // fsw -FP64 store
                        when(C_RVD){
                            io.ctrlSig.lsuSig.dataType  := 0.U
                        }.otherwise{
                            io.illegalInsn := true.B
                        }
                    }
                    is(4.U, 5.U, 6.U, 7.U){
                        io.illegalInsn := true.B 
                    }
                }

                // sanitize memory bus signals for illegal instr(not sure if needed?? )
                when( io.illegalInsn ){
                    dataReq                     := false.B
                    io.ctrlSig.lsuSig.dataWe    := false.B
                }
            }.otherwise{
                io.illegalInsn := true.B
            }
        }//  is(STORE_FP)

        is(LOAD_FP){
            when( (p.FPU == 1).asBool() ){
                dataReq                         := true.B
                io.ctrlSig.regfileSig.memWe     := true.B
                io.regUsed.dFP                  := true.B
                io.regUsed.a                    := true.B
                io.ctrlSig.aluSig.operator      := ALUOp.ADD

                // offset from immediate
                io.ctrlSig.aluSig.immBMuxSel    := IMMB_I
                io.ctrlSig.aluSig.opBMuxSel     := OP_B_IMM

                // NaN boxing
                io.ctrlSig.lsuSig.dataSignExtension := 2.U

                // Decode data type
                switch( io.insnRdata(14,12) ){
                    is(0.U){
                        // fsb -FP8 load
                        when(C_XF8){
                            io.ctrlSig.lsuSig.dataType  := 2.U
                        }.otherwise{
                            io.illegalInsn := true.B
                        }
                    }
                    is(1.U){
                        // fsb -FP16 load
                        when(C_XF16 | C_XF16ALT){
                            io.ctrlSig.lsuSig.dataType  := 1.U
                        }.otherwise{
                            io.illegalInsn := true.B
                        }
                    }
                    is(2.U){
                        // fsw -FP32 load
                        when(C_RVF){
                            io.ctrlSig.lsuSig.dataType  := 0.U
                        }.otherwise{
                            io.illegalInsn := true.B
                        }
                    }
                    is(3.U){
                        // fsw -FP64 load
                        when(C_RVD){
                            io.ctrlSig.lsuSig.dataType  := 0.U
                        }.otherwise{
                            io.illegalInsn := true.B
                        }
                    }
                    is(4.U, 5.U, 6.U, 7.U){
                        io.illegalInsn := true.B 
                    }
                }

                // sanitize memory bus signals for illegal instr(not sure if needed?? )
                when( io.illegalInsn ){
                    dataReq                     := false.B
                    io.ctrlSig.lsuSig.dataWe    := false.B
                }
            }.otherwise{
                io.illegalInsn := true.B
            }
        } //is(LOAD_FP)

        // don`t implement OPCODE_PULP_OP,OPCODE_VECOP 

        ///////////////////////////////////////////
        ////SPECIAL
        ///////////////////////////////////////////

        is(FENCE){
            when( io.insnRdata(14,12) === 0.U){
                // FENCE (FENCE.I instead, a bit more conservative)
                // flush pipeline
                io.fenceiInsn       := true.B
            }.elsewhen( io.insnRdata(14,12) === 1.U){
                // FENCE.I
                // flush prefetch buffer, flush pipeline
                io.fenceiInsn       := true.B
            }.otherwise{
                io.illegalInsn      := true.B
            }
        }//is(FENCE)

        is(SYSTEM){
            when( io.insnRdata(14,12) === 0.U ){
                // non CSR related SYSTEM instructions
                when( Cat(io.insnRdata(9,15), io.insnRdata(11,7)) === 0.U ){
                    when(io.insnRdata(31,20) === "h000".U){ //ECALL
                        // environment (system) call
                        io.eCallInsn        := true.B
                    }.elsewhen(io.insnRdata(31,20) === "h001".U){// ebreak
                        //debugger trap
                        io.eBrkInsn         := true.B
                    }.elsewhen(io.insnRdata(31,20) === "h302".U){//mret
                        io.illegalInsn      := false.B
                        io.retInsn.mret     := ~io.illegalInsn
                        io.retInsn.mret_dec := true.B
                    }.elsewhen(io.insnRdata(31,20) === "h002".U){//uret
                        io.illegalInsn      := true.B
                        io.retInsn.uret     := ~io.illegalInsn
                        io.retInsn.uret_dec := true.B                        
                    }.elsewhen(io.insnRdata(31,20) === "h7b2".U){// dret
                        io.illegalInsn      := !io.debugMode
                        io.retInsn.dret     := io.debugMode
                        io.retInsn.dret_dec := true.B 
                    }.elsewhen(io.insnRdata(31,20) === "h105".U){// wfi
                        // Treat as NOP (do not cause sleep mode entry)
                        // Using decoding similar to ADDI, But without register reads/writes i.e.
                        // Keep io.ctrlSig.regfileSig.aluWe = 0, io.regUsed.a = 0
                        io.ctrlSig.aluSig.opBMuxSel     := OP_B_IMM
                        io.ctrlSig.aluSig.immBMuxSel    := IMMB_I
                        io.ctrlSig.aluSig.operator      := ALUOp.ADD
                    }.otherwise{
                        io.illegalInsn          := true.B
                    }
                }.otherwise{
                    io.illegalInsn          := true.B
                }
            }.otherwise({
                // intruction to read/modify CSR
                io.ctrlSig.csrSig.access                := true.B
                io.ctrlSig.regfileSig.aluWe             := true.B
                io.ctrlSig.aluSig.opBMuxSel             := OP_B_IMM
                io.ctrlSig.aluSig.immAMuxSel            := IMMA_Z
                io.ctrlSig.aluSig.immBMuxSel            := IMMB_I // CSR address is encode in I imm

                when( io.insnRdata(14) === 1.U ){
                    // rs1 field is used as immediate
                    io.ctrlSig.aluSig.opAMuxSel     := OP_A_IMM
                }.otherwise{
                    io.regUsed.a                    := true.B
                    io.ctrlSig.aluSig.opAMuxSel     := OP_A_REGA_OR_FWD
                }
                //  io.insnRdata(19,14) = rs or immediate value
                //  if set or clear with rs === x0 or imm === 0
                //  then do not perform a write action
                switch( io.insnRdata(13,12) ){
                    is(1.U){
                        io.ctrlSig.csrSig.op    := CSR_OP_WRITE
                    }
                    is(2.U){
                        io.ctrlSig.csrSig.op    := Mux(io.insnRdata(19,15) === 0.U, CSR_OP_READ, CSR_OP_SET )
                    }
                    is(3.U){
                        io.ctrlSig.csrSig.op    := Mux(io.insnRdata(19,15) === 0.U, CSR_OP_READ, CSR_OP_CLEAR )
                    }
                    is(0.U){
                        csrIllegal              := true.B
                    }
                }

                when( io.insnRdata(29,28) > io.currentPrivLvl ){
                    csrIllegal := true.B
                }.otherwise{
                    csrIllegal := false.B
                }

                                                // Determine if CSR access is illegal
                when((io.insnRdata(31,20) === CSR_FFLAGS) |
                          (io.insnRdata(31,20) === CSR_FRM) |
                          (io.insnRdata(31,20) === CSR_FCSR)){
                    //FP
                    csr_illegal := Mux((p.FPU == 1).asBool(), false.B, true.B)
                }.elsewhen((io.insnRdata(31,20) === CSR_MVENDORID) |
                              (io.insnRdata(31,20) === CSR_MARCHID) |
                              (io.insnRdata(31,20) === CSR_MIMPID) |
                              (io.insnRdata(31,20) === CSR_MHARTID)){
                    // Writes to read only CSRs results in illegal instruction
                    csr_illegal := Mux(csrOp === CSR_OP_READ, false.B, true.B)
                }.elsewhen((io.insnRdata(31,20) === CSR_MSTATUS) |
                              (io.insnRdata(31,20) === CSR_MEPC) |
                              (io.insnRdata(31,20) === CSR_MTVEC) |
                              (io.insnRdata(31,20) === CSR_MCAUSE)){
                    // These are valid CSR registers
                    // Not illegal, but treat as status CSR for side effect handling
                    io.csr_status_o := true.B
                }.elsewhen((io.insnRdata(31,20) === CSR_MISA) |
                              (io.insnRdata(31,20) === CSR_MIE) |
                              (io.insnRdata(31,20) === CSR_MSCRATCH) |
                              (io.insnRdata(31,20) === CSR_MTVAL) |
                              (io.insnRdata(31,20) === CSR_MIP)){
                    // do nothing, not illegal
                }.elsewhen(io.insnRdata(31,20) === CSR_CYCLE 
                |  io.insnRdata(31,20) === CSR_INSTRET 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER3 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER4   |  io.insnRdata(31,20) === CSR_HPMCOUNTER5   |  io.insnRdata(31,20) === CSR_HPMCOUNTER6   |  io.insnRdata(31,20) === CSR_HPMCOUNTER7 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER8   |  io.insnRdata(31,20) === CSR_HPMCOUNTER9   |  io.insnRdata(31,20) === CSR_HPMCOUNTER10  |  io.insnRdata(31,20) === CSR_HPMCOUNTER11 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER12  |  io.insnRdata(31,20) === CSR_HPMCOUNTER13  |  io.insnRdata(31,20) === CSR_HPMCOUNTER14  |  io.insnRdata(31,20) === CSR_HPMCOUNTER15 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER16  |  io.insnRdata(31,20) === CSR_HPMCOUNTER17  |  io.insnRdata(31,20) === CSR_HPMCOUNTER18  |  io.insnRdata(31,20) === CSR_HPMCOUNTER19 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER20  |  io.insnRdata(31,20) === CSR_HPMCOUNTER21  |  io.insnRdata(31,20) === CSR_HPMCOUNTER22  |  io.insnRdata(31,20) === CSR_HPMCOUNTER23 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER24  |  io.insnRdata(31,20) === CSR_HPMCOUNTER25  |  io.insnRdata(31,20) === CSR_HPMCOUNTER26  |  io.insnRdata(31,20) === CSR_HPMCOUNTER27 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER28  |  io.insnRdata(31,20) === CSR_HPMCOUNTER29  |  io.insnRdata(31,20) === CSR_HPMCOUNTER30  |  io.insnRdata(31,20) === CSR_HPMCOUNTER31 
                |  io.insnRdata(31,20) === CSR_CYCLEH 
                |  io.insnRdata(31,20) === CSR_INSTRETH 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER3H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER4H   |  io.insnRdata(31,20) === CSR_HPMCOUNTER5H   |  io.insnRdata(31,20) === CSR_HPMCOUNTER6H   |  io.insnRdata(31,20) === CSR_HPMCOUNTER7H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER8H   |  io.insnRdata(31,20) === CSR_HPMCOUNTER9H   |  io.insnRdata(31,20) === CSR_HPMCOUNTER10H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER11H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER12H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER13H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER14H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER15H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER16H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER17H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER18H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER19H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER20H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER21H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER22H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER23H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER24H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER25H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER26H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER27H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER28H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER29H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER30H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER31H){
                    // Not illegal, but treat as status CSR to get accurate counts
                    io.ctrlSig.csrSig.status := true.B
                }.elsewhen(io.insnRdata(31,20) === CSR_CYCLE 
                |  io.insnRdata(31,20) === CSR_INSTRET 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER3 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER4   |  io.insnRdata(31,20) === CSR_HPMCOUNTER5   |  io.insnRdata(31,20) === CSR_HPMCOUNTER6   |  io.insnRdata(31,20) === CSR_HPMCOUNTER7 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER8   |  io.insnRdata(31,20) === CSR_HPMCOUNTER9   |  io.insnRdata(31,20) === CSR_HPMCOUNTER10  |  io.insnRdata(31,20) === CSR_HPMCOUNTER11 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER12  |  io.insnRdata(31,20) === CSR_HPMCOUNTER13  |  io.insnRdata(31,20) === CSR_HPMCOUNTER14  |  io.insnRdata(31,20) === CSR_HPMCOUNTER15 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER16  |  io.insnRdata(31,20) === CSR_HPMCOUNTER17  |  io.insnRdata(31,20) === CSR_HPMCOUNTER18  |  io.insnRdata(31,20) === CSR_HPMCOUNTER19 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER20  |  io.insnRdata(31,20) === CSR_HPMCOUNTER21  |  io.insnRdata(31,20) === CSR_HPMCOUNTER22  |  io.insnRdata(31,20) === CSR_HPMCOUNTER23 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER24  |  io.insnRdata(31,20) === CSR_HPMCOUNTER25  |  io.insnRdata(31,20) === CSR_HPMCOUNTER26  |  io.insnRdata(31,20) === CSR_HPMCOUNTER27 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER28  |  io.insnRdata(31,20) === CSR_HPMCOUNTER29  |  io.insnRdata(31,20) === CSR_HPMCOUNTER30  |  io.insnRdata(31,20) === CSR_HPMCOUNTER31 
                |  io.insnRdata(31,20) === CSR_CYCLEH 
                |  io.insnRdata(31,20) === CSR_INSTRETH 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER3H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER4H   |  io.insnRdata(31,20) === CSR_HPMCOUNTER5H   |  io.insnRdata(31,20) === CSR_HPMCOUNTER6H   |  io.insnRdata(31,20) === CSR_HPMCOUNTER7H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER8H   |  io.insnRdata(31,20) === CSR_HPMCOUNTER9H   |  io.insnRdata(31,20) === CSR_HPMCOUNTER10H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER11H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER12H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER13H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER14H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER15H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER16H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER17H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER18H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER19H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER20H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER21H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER22H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER23H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER24H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER25H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER26H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER27H 
                |  io.insnRdata(31,20) === CSR_HPMCOUNTER28H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER29H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER30H  |  io.insnRdata(31,20) === CSR_HPMCOUNTER31H){
                    // Read-only and readable from user mode only if the bit of mcounteren is set
                    io.ctrlSig.csrSig.status := Mux(csr_op != CSR_OP_READ, true.B, false.B)
                }.elsewhen(io.insnRdata(31,20) === CSR_MCOUNTEREN){
                    // This register only exists in user mode
                    csrIllegal := true.B
                }.elsewhen((io.insnRdata(31,20) === CSR_DCSR) |
                              (io.insnRdata(31,20) === CSR_DPC) |
                              (io.insnRdata(31,20) === CSR_DSCRATCH0) |
                              (io.insnRdata(31,20) === CSR_DSCRATCH1)){
                    // Debug register access
                    when(~io.debug_mode_i){
                        csrIllegal := true.B
                    }.otherwise(){
                        io.ctrlSig.csrSig.status := true.B
                    }
                }.elsewhen((io.insnRdata(31,20) === CSR_TSELECT) |
                              (io.insnRdata(31,20) === CSR_TDATA1) |
                              (io.insnRdata(31,20) === CSR_TDATA2) |
                              (io.insnRdata(31,20) === CSR_TDATA3) |
                              (io.insnRdata(31,20) === CSR_TINFO) |
                              (io.insnRdata(31,20) === CSR_MCONTEXT) |
                              (io.insnRdata(31,20) === CSR_SCONTEXT)){
                    // Debug Trigger register access
                    csr_illegal := Mux( DEBUG_TRIGGER_EN , false.B, true.B)
                }.elsewhen((io.insnRdata(31,20) === CSR_LPSTART0) |
                              (io.insnRdata(31,20) === CSR_LPEND0) |
                              (io.insnRdata(31,20) === CSR_LPCOUNT0) |
                              (io.insnRdata(31,20) === CSR_LPSTART1) |
                              (io.insnRdata(31,20) === CSR_LPEND1) |
                              (io.insnRdata(31,20) === CSR_LPCOUNT1) |
                              (io.insnRdata(31,20) === CSR_UHARTID)){
                    // Hardware loop register, we currently do not implement
                    csr_illegal := true.B
                }.elsewhen(io.insnRdata(31,20) === CSR_PRIVLV){
                    //PRIVILV access
                    csr_illegal := true.B
                }.elsewhen((io.insnRdata(31,20) === CSR_PMPCFG0) | (io.insnRdata(31,20) === CSR_PMPCFG1) |
                              (io.insnRdata(31,20) === CSR_PMPCFG2) | (io.insnRdata(31,20) === CSR_PMPCFG3) |
                              (io.insnRdata(31,20) === CSR_PMPADDR0) | (io.insnRdata(31,20) === CSR_PMPADDR1) |
                              (io.insnRdata(31,20) === CSR_PMPADDR2) | (io.insnRdata(31,20) === CSR_PMPADDR3) |
                              (io.insnRdata(31,20) === CSR_PMPADDR4) | (io.insnRdata(31,20) === CSR_PMPADDR5) |
                              (io.insnRdata(31,20) === CSR_PMPADDR6) | (io.insnRdata(31,20) === CSR_PMPADDR7) |
                              (io.insnRdata(31,20) === CSR_PMPADDR8) | (io.insnRdata(31,20) === CSR_PMPADDR9) |
                              (io.insnRdata(31,20) === CSR_PMPADDR10) | (io.insnRdata(31,20) === CSR_PMPADDR11) |
                              (io.insnRdata(31,20) === CSR_PMPADDR12) | (io.insnRdata(31,20) === CSR_PMPADDR13) |
                              (io.insnRdata(31,20) === CSR_PMPADDR14) | (io.insnRdata(31,20) === CSR_PMPADDR15)){
                    // PMP register access
                    csrIllegal  := Mux((p.USE_PMP != 1).asBool(), true.B, false.B)
                }.elsewhen((io.insnRdata(31,20) === CSR_USTATUS) | (io.insnRdata(31,20) === CSR_UEPC) | (io.insnRdata(31,20) === CSR_UTVEC) | (io.insnRdata(31,20) === CSR_UCAUSE)){
                    // User register access
                        csr_illegal := true.B
                }.otherwise{
                    csrIllegal      := true.B
                }

                io.illegalInsn     := csrIllegal
            })
        }//is(SYSTEM)

        //////////////////////////////////////////////
        ////HWLOOP don`t implement
        //////////////////////////////////////////////

    }//switch(io.insnRdata(6,0))
    
    // make sure invalid compressed instruction causes an exception
    when(io.illegalCInsn){
        io.illegalInsn          := true.B
    }

    // deassert we signals (in case of stalls)
    aluEn                               := Mux(io.deassertWe, false.B, aluEn)
    apuEn                               := Mux(io.deassertWe, false.B, apuEn)
    io.ctrlSig.mulSig.intEn             := Mux(io.deassertWe, false.B, io.ctrlSig.mulSig.intEn)
    io.ctrlSig.regfileSig.memWe         := Mux(io.deassertWe, false.B, io.ctrlSig.regfileSig.memWe)
    io.ctrlSig.regfileSig.aluWe         := Mux(io.deassertWe, false.B, io.ctrlSig.regfileSig.aluWe)
    io.ctrlSig.lsuSig.dataReq           := Mux(io.deassertWe, false.B, io.ctrlSig.lsuSig.dataReq)
    io.ctrlSig.csrSig.op                := Mux(io.deassertWe, CSR_OP_READ, csrOp)
    io.ctrlTransferInsnInID             := Mux(io.deassertWe, BRANCH_NONE, ctrlTransferInsn)

    io.ctrlTransferInsnInDec            := ctrlTransferInsn
    io.ctrlSig.regfileSig.aluWeDec      := io.ctrlSig.regfileSig.aluWe
    






}//class Decoder()
