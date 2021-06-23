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
  *     - prefetch controller: prefetch_controller.scala
  */
class FetchStageInf()(p: IFParams) extends Bundle {
    val req           = Input(Bool())     // Fetch stage requests instructions
    val branch        = Input(Bool())     // Taken branch
    val branch_addr   = Input(UInt(32.W)) // Taken branch address (only valid when branch = 1)
    val busy          = Output(Bool())    // Prefetcher busy   
}

/**
  * Transaction request and response interface:
  *     - prefetch controller: prefetch_controller.scala
  */
class TransactionInf()(p: IFParams) extends Bundle {
    /** Transaction request interface */
    val transRequest = DecoupledIO(new Bundle{ val addr = UInt(32.W) })
    /** Transaction response interface */
    val transRespValid = Input(Bool())
}

/**
  * FIFO interface:
  *     - prefetch controller: prefetch_controller.scala
  */
class FIFOInf()(p: IFParams) extends Bundle {
    val fifo_push   = Output(Bool())
    val fifo_pop    = Output(Bool())
    val fifo_flush  = Output(Bool())            // Flush the FIFO
    val fifo_flush_but_first = Output(Bool())   // Flush the FIFO, but keep the first instruction if present
    val fifo_cnt    = Input(UInt(p.FIFO_ADDR_DEPTH.W))
    val fifo_empty  = Input(Bool())
}
