/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-23
  * Description: Defines IO interfaces for common usages.
  */

package megumi32.include

import chisel3._
import chisel3.util._

/**
  * Fetch stage interface:
  *     - prefetch controller: PrefetchController.scala
  */
class FetchStageInf()(p: IFParams) extends Bundle {
    val req           = Input(Bool())     // Fetch stage requests instructions
    val branch        = Input(Bool())     // Taken branch
    val branch_addr   = Input(UInt(32.W)) // Taken branch address (only valid when branch = 1)
    val busy          = Output(Bool())    // Prefetcher busy   
}

/**
  * Transaction request and response interface:
  *     - prefetch controller: PrefetchController.scala
  */
class TransactionInf()(p: IFParams) extends Bundle {
    /** Transaction request interface */
    val transRequest = DecoupledIO(new Bundle{ val addr = UInt(32.W) })
    /** Transaction response interface */
    val transResp_valid = Input(Bool())
}

/**
  * FIFO interface:
  *     - prefetch controller: PrefetchController.scala
  */
class FIFOInf()(p: IFParams) extends Bundle {
    val fifo_push   = Output(Bool())
    val fifo_pop    = Output(Bool())
    val fifo_flush  = Output(Bool())            // Flush the FIFO
    val fifo_flush_but_first = Output(Bool())   // Flush the FIFO, but keep the first instruction if present
    val fifo_cnt    = Input(UInt(p.FIFO_ADDR_DEPTH.W))
    val fifo_empty  = Input(Bool())
}

/**
  * Transaction request and response interface extended (Master sides):
  *     - OBI interface: ObiInterface.scala
  * 
  * Usages:
  *     Prefetch Controller (M) <-> OBI interface (S)
  */
class TransactionExInf()(p: IFParams) extends TransactionInf()(p) {
    /** Extend signals for transactions request interface */
    val transRequest_we        = Output(Bool())
    val transRequest_be        = Output(UInt(p.BE_WIDTH.W))
    val transRequest_wdata     = Output(UInt(p.XLEN.W))
    val transRequest_atop      = Output(UInt(p.ATOP_WDITH.W))   // Future proof addtion, no use here

    /** Transaction response interface extend */
    val transResp_rdata        = Input(UInt(p.XLEN.W))
    val transResp_err          = Input(Bool())
}

/**
  * OBI (Master) interface: Standard interface of PULP series cores
  * Use Flipped() for Slave sides
  *
  * @param p: T -> T must be subclass of BasicParams
  */
class ObiInf[T <: BasicParams]()(p: T) extends Bundle {
    /** A channel signals */
    val req         = Output(Bool())                // Address transfer request       
    val gnt         = Input(Bool())                 // Grant. Ready to accept address transfer
    val addr        = Output(UInt(p.XLEN.W))        // Address
    val we          = Output(Bool())                // Write enable. 1 for writes, 0 for reads
    val be          = Output(UInt(p.BE_WIDTH.W))    // Byte enable. Is set for the bytes write/read
    val wdata       = Output(UInt(p.XLEN.W))        // Write data
    val atop        = Output(UInt(p.ATOP_WDITH.W))

    /** R channel signals */
    val rdata       = Input(UInt(p.XLEN.W))         // Read data
    val rvalid      = Input(Bool())                 // Response transfer request
    val err         = Input(Bool())                 // Error
}

/**
  * Instrcution memory/cache interface: Simplify OBI interface (Master)
  * 
  * @param p: T -> T must be subclass of BasicParams
  */
class InstrInf[T <: BasicParams]()(p: T) extends Bundle {
    val req         = Output(Bool())                // Address transfer request       
    val gnt         = Input(Bool())                 // Grant. Ready to accept address transfer
    val addr        = Output(UInt(p.XLEN.W))        // Address
    val rdata       = Input(UInt(p.XLEN.W))         // Read data
    val rvalid      = Input(Bool())                 // Response transfer request
}

/**
  * IO interface for IF stage, used to calculate the exception offsets.
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class TrapBaseAddr[T <: BasicParams]()(p: T) extends Bundle {
    val maddr = Input(UInt(p.TRAP_LEN.W))
    val uaddr = Input(UInt(p.TRAP_LEN.W))
    val mux   = Input(UInt(2.W))
}

/**
  * IF/ID Pipeline Registers IO interface
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class IFPipelineReg[T <: BasicParams]()(p: T) extends Bundle {
    val instrValid_ID   = Bool()            // Instruction in IF/ID pipeline is valid
    val instrRdata_ID   = UInt(p.XLEN.W)    // Read instruction is sampled and sent to ID stage for decoding
    val isCompressed_ID = Bool()            // Compressed decoder thinks this is a compressed instruction
    val illegalCInsn_ID = Bool()            // Compressed decoder thinks this is an invalid instruction
    val pc_IF           = UInt(p.XLEN.W)
    val pc_ID           = UInt(p.XLEN.W)
    val isFetchFailed   = Bool()

    def stall(): Unit = {
        instrValid_ID   := instrValid_ID
        instrRdata_ID   := instrRdata_ID
        isCompressed_ID := isCompressed_ID
        illegalCInsn_ID := illegalCInsn_ID
        pc_IF           := pc_IF
        pc_ID           := pc_ID
        isFetchFailed   := isFetchFailed
    }
}

/**
  * Address used to restore PC when the interrupt/execption is served
  * IO interfaces
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class Xepc[T <: BasicParams]()(p: T) extends Bundle {
    val mepc = Input(UInt(p.XLEN.W))
    val uepc = Input(UInt(p.XLEN.W))
    val depc = Input(UInt(p.XLEN.W))
}

/**
  * Return from exception instruction/debug encountered & without deassert
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class RetInsn[T <: BasicParams]()(p: T) extends Bundle {
    val mret        = Bool()    // Return from exception instruction encountered (M)
    val uret        = Bool()    // Return from exception instruction encountered (U)
    val dret        = Bool()    // Return from debug (M)

    val mret_dec    = Bool()    // Return from exception instruction encountered (M) without deassert
    val uret_dec    = Bool()    // Return from exception instruction encountered (U) without deassert 
    val dret_dec    = Bool()    // Return from debug (M) without deassert
}

/**
  * Registers used signals
  *
  * @param p: ID stage parameters
  */
class RegUsed()(p: IDParams) extends Bundle {
    val a   = Bool()    // rs1 is used by current instruction
    val b   = Bool()    // rs2 is used by current instruction
    val c   = Bool()    // rs3 is used by current instruction
    val aFP = Bool()    // fp reg a is used
    val bFP = Bool()    // fp reg b is used
    val cFP = Bool()    // fp reg c is used
    val dFP = Bool()    // fp reg d is used
}

/**
  * ALU control signals
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class ALUSig[T <: BasicParams]()(p: T) extends Bundle {
    val en              = Bool()                // ALU enable
    val operator        = UInt(ALUOp.width.W)   // ALU operation selection
    val opAMuxSel       = UInt(3.W)             // Operand A selection: reg value, PC, immediate or zero
    val opBMuxSel       = UInt(3.W)             // Operand B selection: reg value or immediate
    val opCMuxSel       = UInt(2.W)             // Operand C selection: reg value or jump target
    // val aluVecMode      = UInt(2.W)             // Selects between 32/16/8 bit vectorial modes
    // val scalarReplication = Bool()              // Scalar replication enable
    // val scalarReplicationC = Bool()             // Scalar replication enable for operand C
    val immAMuxSel      = Bool()                // immediate selection for operand A
    val immBMuxSel      = UInt(4.W)             // immediate selection for operand B
    val regCMux         = UInt(2.W)             // Register C selection: S3, RD or 0
    // val isClpx          = Bool()                // Whether the instruction is complex or not
    // val isSubrot        = Bool()
}

/**
  * MUL related control signals
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class MULSig[T <: BasicParams]()(p: T) extends Bundle {
    val operator        = UInt(MULop.width.W)   // Multiplication operation selection
    val intEn           = Bool()                // Perform integer multiplication
    // val dotEn           = Bool()                // Perfrom dot multiplication
    val immMux          = Bool()                // Multiplication immediate mux selector 
    val selSubword      = Bool()                // Select subwords for 16x16 bit for multiplier
    val signedMode      = UInt(2.W)             // Multiplication signed mode
    // val dotSigned       = UInt(2.W)             // Dot product in signed mode
}

/**
  * FPU related control signals
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class FPUSig[T <: BasicParams]()(p: T) extends Bundle {
    val frm             = Input(UInt(FPU.C_RM.W))               // Rounding mode from float CSR

    val dstFmt          = Output(UInt(FPU.FP_FORMAT_BITS.W))    // FPU destination format
    val srcFmt          = Output(UInt(FPU.FP_FORMAT_BITS.W))    // FPU source format
    val intFmt          = Output(UInt(FPU.INT_FORMAT_BITS.W))   // FPU integer format (for casts)
}

/**
  * Register file related signals
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class RegfileSig[T <: BasicParams]()(p: T) extends Bundle {
    val memWe           = Bool()        // Write enable for regfile
    val aluWe           = Bool()        // Write enable for 2nd regfile port
    val aluWeDec        = Bool()        // Write enable for 2nd regfile port without deassert
    val aluWaddrSel     = Bool()        // Select register write address for ALU/MUL operation
}

/**
  * CSR manipulation signals
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class CSRSig[T <: BasicParams]()(p: T) extends Bundle {
    val access          = Bool()                // Access to CSR
    val status          = Bool()                // Access to xstatus CSR
    val op              = UInt(CSROp.width.W)   // Operation to perform on CSR
    val currentPrivLvl  = UInt(PrivLvl.width.W) // The current privilege level
}

/**
  * LD/ST unit signals
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class LSUSig[T <: BasicParams]()(p: T) extends Bundle {
    val dataReq             = Bool()                // Start transaction to data memory
    val dataWe              = Bool()                // Data memory write enable
    val prepostUseIncr      = Bool()                // When not active bypass the alu result for address calculation
    val dataType            = UInt(2.W)             // Data type on data memory: byte, half word or word
    val dataSignExtension   = UInt(2.W)             // Sign extension on read data from data memory / NaN boxing
    val dataRegOffset       = UInt(2.W)             // Offset in byte inside register for stores
    val dataLoadEvent       = Bool()                // Data request is in the special event range
}

/**
  * Control signals interface
  *
  * @param p: subclass of BasicParams -> Parameters for the module
  */
class CtrlSig[T <: BasicParams]()(p: T) extends Bundle {
    val aluSig      = Output(new ALUSig()(p))       // ALU signals
    val mulSig      = Output(new MULSig()(p))       // MULT signals
    val fpuSig      = new FPUSig()(p)               // FPU signals
    val regfileSig  = Output(new RegfileSig()(p))   // Register file related signals
    val csrSig      = Output(new CSRSig()(p))       // CSR manipulation signals
    val lsuSig      = Output(new LSUSig()(p))       // LD/ST unit signals
    val atop        = Output(UInt(6.W))             // Atomic memory access
}
