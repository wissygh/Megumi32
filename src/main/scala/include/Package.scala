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

class IFParams() extends BasicParams {
    val DEPTH: Int           = 4           // Prefetch FIFO Depth
    val FIFO_ADDR_DEPTH: Int = if (DEPTH > 1) log2Ceil(DEPTH) else 1
}

class FIFOParams() extends IFParams {
    val FALL_THROUGH: UInt      = 0.U(1.W)  // FIFO is in fall-through mode
    val DATA_WIDTH: Int         = 32        // Default data width if the fifo is of type logic
    val ADDR_DEPTH: Int         = FIFO_ADDR_DEPTH
}
