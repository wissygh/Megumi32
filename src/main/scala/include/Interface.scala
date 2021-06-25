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
