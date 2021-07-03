/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-07-02
  * Description: Instruction fetch stage: Selection of the next PC, 
  *              and buffering (sampling) of the read instruction.
  */
package megumi32.rtl

import chisel3._
import chisel3.util._
import megumi32.include._
import chisel3.stage.ChiselStage

class IfStageIO()(p: IFParams) extends Bundle {
    /** Used to calculate the exception offsets */
    val trapBaseAddr = new TrapBaseAddr()(p)
    
    /** Boot address */
    val bootAddr        = Input(UInt(p.XLEN.W))
    val dmExceptionAddr = Input(UInt(p.XLEN.W))

    /** Debug mode halt address */
    val dmHaltAddr      = Input(UInt(p.XLEN.W))

    /** Instruction request control */
    val req             = Input(Bool())

    /** Instruction cache interface */
    val instrInf        = new InstrInf()(p)

    /** Output of IF Pipeline Stage */
    val ifPipelineReg   = Output(new IFPipelineReg()(p))

    /** Forwarding ports - control signals */
    val clearInstrValid = Input(Bool())     // Clear instruction valid bit in IF/ID pipeline
    val pcSet           = Input(Bool())     // Set the program counter to a new value

    val xepc            = new Xepc()(p)

    val pcMux           = Input(UInt(4.W))  // Sel for PC multipliexer
    val excPcMux        = Input(UInt(3.W))  // Selects ISR address

    val mExcVecPcMux    = Input(UInt(5.W))  // Selects ISR address for vectorized interrupt lines
    val uExcVecPcMux    = Input(UInt(5.W))
    val csrMtvecInit    = Output(Bool())    // Tell CS regfile to init mtvec

    /** Jump and branch target and decision */
    val jumpTargetID    = Input(UInt(p.XLEN.W))     // Jump target address
    val jumpTargetEX    = Input(UInt(p.XLEN.W))     // Jump target address

    /** Pipeline stall */
    val haltIF          = Input(Bool())
    val readyID         = Input(Bool())

    /** misc signals */
    val busyIF          = Output(Bool())            // Is the IF stage busy fetching instructions?
    val perfIMiss       = Output(Bool())            // Instruction fetch miss
}

class IfStage()(p: IFParams) extends Module with RequireAsyncReset {
    val io = IO(new IfStageIO()(p))

    /** Local signals */
    val validIF, readyIF = Wire(Bool())

    /** Prefetch buffer related signals */
    val prefetchBusy        = Wire(Bool())
    val branchReq           = Wire(Bool())
    val branchAddrn         = Wire(UInt(p.XLEN.W))

    val fetchValid          = Wire(Bool())
    val fetchReady          = Wire(Bool())
    val fetchRdata          = Wire(UInt(p.XLEN.W))

    val excPc               = Wire(UInt(p.XLEN.W))

    val trapBaseAddr        = Wire(UInt(p.TRAP_LEN.W))
    val excVecPcMux         = Wire(UInt(5.W))
    val fetchFailed         = Wire(Bool())

    val alignerReady        = Wire(Bool())
    val instrValid          = Wire(Bool())

    val illegalCInsn        = Wire(Bool())
    val instrAligned        = Wire(UInt(p.XLEN.W))
    val instrDecompressed   = Wire(UInt(p.XLEN.W))
    val instrCompressedInt  = Wire(Bool())

    /** Exception PC selection Mux */
    trapBaseAddr := MuxLookup(io.trapBaseAddr.mux, io.trapBaseAddr.maddr, Array(
        p.TRAP_MACHINE -> io.trapBaseAddr.maddr,
        p.TRAP_USER    -> io.trapBaseAddr.uaddr
    ))

    excVecPcMux := MuxLookup(io.trapBaseAddr.mux, io.mExcVecPcMux, Array(
        p.TRAP_MACHINE -> io.mExcVecPcMux,
        p.TRAP_USER    -> io.uExcVecPcMux
    ))

    excPc := MuxLookup(io.excPcMux, Cat(trapBaseAddr, 0.U(8.W)), Array(
        p.EXC_PC_EXCEPTION -> Cat(trapBaseAddr, 0.U(8.W)),
        p.EXC_PC_IRQ       -> Cat(trapBaseAddr, 0.U(1.W), excVecPcMux, 0.U(2.W)),
        p.EXC_PC_DBD       -> Cat(io.dmHaltAddr(31, 2), 0.U(2.W)),
        p.EXC_PC_DBE       -> Cat(io.dmExceptionAddr(31, 2), 0.U(2.W))
    ))

    /** Fetch address selection */
    branchAddrn := MuxLookup(io.pcMux, Cat(io.bootAddr(31, 2), 0.U(2.W)), Array(
        p.PC_BOOT       -> Cat(io.bootAddr(31, 2), 0.U(2.W)),
        p.PC_JUMP       -> io.jumpTargetID,
        p.PC_BRANCH     -> io.jumpTargetEX,
        p.PC_EXCEPTION  -> excPc,           // Set PC to exception handler
        p.PC_MRET       -> io.xepc.mepc,    // PC is restored when returning from IRQ/exception
        p.PC_URET       -> io.xepc.uepc,
        p.PC_DRET       -> io.xepc.depc,
        p.PC_FENCEI     -> (io.ifPipelineReg.pc_ID + 4.U),  // Jump to next instruction forces prefetch buffer reload
    ))
    
    /** Tell CS regsiter file to initialize mtvec on boot */
    io.csrMtvecInit := (io.pcMux === p.PC_BOOT) & io.pcSet

    /** PMP is not supported in our implementation */
    fetchFailed := false.B

    /////////////////////////////////////////////////////////////
    // Prefetch buffer, caches a fixed number of instructions
    /////////////////////////////////////////////////////////////
    val prefetchBuffer = Module(new PrefetchBuffer()(p))

    /** Fetch interface */
    prefetchBuffer.io.fsInf.req := io.req
    prefetchBuffer.io.fsInf.branch := branchReq
    prefetchBuffer.io.fsInf.branch_addr := Cat(branchAddrn(31, 1), 0.U(1.W))
    prefetchBusy := prefetchBuffer.io.fsInf.busy

    prefetchBuffer.io.fetch_rdata.ready := fetchReady
    fetchValid := prefetchBuffer.io.fetch_rdata.valid
    fetchRdata := prefetchBuffer.io.fetch_rdata.bits

    prefetchBuffer.io.instrInf <> io.instrInf

    /** offset FSM state transition logic */
    fetchReady := false.B
    branchReq := false.B
    /** Take care of jumps and branches */
    when(io.pcSet) {
        branchReq := true.B
    }.elsewhen(fetchValid) {
        when(io.req && validIF) {
            fetchReady := alignerReady
        }
    }

    io.busyIF := prefetchBusy
    io.perfIMiss := !fetchValid && !branchReq

    /** IF/ID pipeline registers, frozen when the ID stage is stalled */
    val pipelineRegs = RegInit(0.U.asTypeOf(new IFPipelineReg()(p)))
 
    when(validIF && instrValid) {
        pipelineRegs.instrValid_ID      := true.B
        pipelineRegs.instrRdata_ID      := instrDecompressed
        pipelineRegs.isCompressed_ID    := instrCompressedInt
        pipelineRegs.illegalCInsn_ID    := illegalCInsn
        pipelineRegs.isFetchFailed      := false.B
        pipelineRegs.pc_ID              := pipelineRegs.pc_IF
    }.elsewhen(io.clearInstrValid) {
        pipelineRegs.instrValid_ID := false.B
        pipelineRegs.isFetchFailed := fetchFailed
    }.otherwise { pipelineRegs.stall }

    readyIF := fetchValid & io.readyID
    validIF := (~io.haltIF) & readyIF

    /////////////////////////////////////////////////////////////
    // Aligner
    /////////////////////////////////////////////////////////////
    val aligner = Module(new Aligner()(p))

    aligner.io.fetch_valid := fetchValid
    alignerReady := aligner.io.aligner_ready
    aligner.io.if_valid := validIF
    aligner.io.fetch_rdata := fetchRdata
    instrAligned := aligner.io.instr_aligned
    instrValid := aligner.io.instr_valid
    aligner.io.branch_addr := Cat(branchAddrn(31, 1), 0.U(1.W))
    aligner.io.branch := branchReq
    pipelineRegs.pc_IF := aligner.io.pc

    /////////////////////////////////////////////////////////////
    // Compressed Decoder
    /////////////////////////////////////////////////////////////
    val cd = Module(new CompressedDecoder()(p))

    cd.io.instr_in := instrAligned
    instrDecompressed := cd.io.instr_out
    instrCompressedInt := cd.io.is_compressed
    illegalCInsn := cd.io.illegal_instr

    /** Pipeline regs to output */
    io.ifPipelineReg <> pipelineRegs
}

object IfStage extends App {
    /**  */
    (new ChiselStage).emitFirrtl(new IfStage()(new IFParams))
}
