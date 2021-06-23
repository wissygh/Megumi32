/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-23
  * Description: Defines for various constants used by the processor core.
  */

package megumi32.include

import chisel3._
import chisel3.util._

trait BasicParams {
    val PULP_OBI: Int = 0        // Legacy PULP OBI behavior
    // val PULP_XPULP = 0   // PULP ISA Extension (no use in our implementation)
}

case class IFParams() extends BasicParams {
    val DEPTH: Int           = 4           // Prefetch FIFO Depth
    val FIFO_ADDR_DEPTH: Int = if (DEPTH > 1) log2Ceil(DEPTH) else 1
}

/**
  * Generate circuit model for register / register next
  * Currently only support naive UInt single register
  */
class RegCouple(width: Int, init: Option[UInt] = None) {
    val q = init match {
        case Some(value) => RegInit(init.get)
        case None        => RegInit(0.U(width.W))
    }
    val n = Wire(UInt(width.W))
}

object RegCouple {
    /** 
     * Simple one register initialization 
     * Although we provide non initial value versions, but we highly recommand
     * to init the register with accurate value.
     */
    def apply(width: Int, init: Option[UInt] = None) = new RegCouple(width, init)
}
