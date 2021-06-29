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

object Opcode {
    val SYSTEM    = 0x73.U(7.W)
    val FENCE     = 0x0f.U(7.W)
    val OP        = 0x33.U(7.W)
    val OPIMM     = 0x13.U(7.W)
    val STORE     = 0x23.U(7.W)
    val LOAD      = 0x03.U(7.W)
    val BRANCH    = 0x63.U(7.W)
    val JALR      = 0x67.U(7.W)
    val JAL       = 0x6f.U(7.W)
    val AUIPC     = 0x17.U(7.W)
    val LUI       = 0x37.U(7.W)
    val OP_FP     = 0x53.U(7.W)
    val OP_FMADD  = 0x43.U(7.W)
    val OP_FNMADD = 0x4f.U(7.W)
    val OP_FMSUB  = 0x47.U(7.W)
    val OP_FNMSUB = 0x4b.U(7.W)
    val STORE_FP  = 0x27.U(7.W)
    val LOAD_FP   = 0x07.U(7.W)
    val AMO       = 0x2F.U(7.W)
}
