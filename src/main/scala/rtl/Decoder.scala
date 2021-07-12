/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-07-09
  * Description: Decoder.
  */
package megumi32.rtl

import chisel3._
import chisel3.util._
import megumi32.include._
import megumi32.include.CtrlOpConstans._
import megumi32.include.ALUOp._
import megumi32.include.MULop._
import megumi32.include.FPU._
import megumi32.include.CSROp._

abstract trait DecodeTable {
    val table: Array[(BitPat, List[UInt])]
}

class IDecode extends DecodeTable {
    val table: Array[(BitPat, List[UInt])] = Array(

    )
}

class DecoderIO()(p: IDParams) extends Bundle {
    val deasserWe           = Input(Bool())

    val illegalInsn         = Output(Bool())                // Illegal instruction encountered
    val eBrkInsn            = Output(Bool())                // Trap instruction encountered

    /** Return from exception/debug instruction encountered & without deassert version */
    val retInsn             = Output(new RetInsn()(p))

    val eCallInsn           = Output(Bool())                // Environment call (syscall) instruction encountered
    val wfi                 = Output(Bool())                // Pipeline flush is requested

    val fenceiInsn          = Output(Bool())                // FENCE.I instruction

    /** Registers used signals */
    val regUsed             = Output(new RegUsed()(p))

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
    val regfileMemWe        = Wire(Bool())
    val regfileAluWe        = Wire(Bool())
    val dataReq             = Wire(Bool())
    val csrIllegal          = Wire(Bool())
    val ctrlTransferInsn    = Wire(UInt(2.W))

    val csrOp               = Wire(UInt(CSROp.width.W))

    val aluEn               = Wire(Bool())
    val multIntEn           = Wire(Bool())

    val checkFprm           = Wire(Bool())
    val fpuOp               = Wire(UInt(FPU.OP_BITS.W))     // FPU operation
    val fpuOpMod            = Wire(Bool())                  // FPU operation modifier
    val fpuVecOp            = Wire(Bool())                  // FPU vectorial operation

}
