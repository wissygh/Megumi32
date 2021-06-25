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
    val PULP_OBI: Int   = 0     // Legacy PULP OBI behavior
    // val PULP_XPULP   = 0     // PULP ISA Extension (no use in our implementation)
    val XLEN: Int       = 32    // Width of an integer register in bits

    /** OBI interface relative.*/
    val BE_WIDTH: Int   = 4     // Byte enable width
    val ATOP_WDITH: Int = 6     // Future proof addtion (no use now)
}

class IFParams(var depth: Option[Int] = None) extends BasicParams {
    var DEPTH: Int           = depth match {
        case Some(value) => value
        case None => 4
    }
    val FIFO_ADDR_DEPTH: Int = if (DEPTH > 1) log2Ceil(DEPTH) else 1
}

class FIFOParams(depth: Option[Int] = None) extends IFParams(depth) {
    val FALL_THROUGH: UInt      = 0.U(1.W)  // FIFO is in fall-through mode
    val DATA_WIDTH: Int         = 32        // Default data width if the fifo is of type logic
    val ADDR_DEPTH: Int         = FIFO_ADDR_DEPTH
}

class ObiParams(depth: Option[Int] = None) extends IFParams(depth) {
    val TRANS_STABLE: Int = 0       // Are trans_addr_i, trans_we_i, trans_be_i, trans_wdata_i, trans_atop_i signals stable during a non-accepted transaction?
}
