/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-24
  * Description: Define util functions
  */
package megumi32.include

import chisel3._
import chisel3.util._

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
