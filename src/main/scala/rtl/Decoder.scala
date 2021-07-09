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

    val debugMode           = Input(Bool())                 // Processor is in debug mode
    val debugWfiNoSleep     = Input(Bool())                 // Do not let WFI cause sleep

    /** Jump/Branches */
    val ctrlTransferInsnInDec = Output(UInt(2.W))           // Control transfer instruction without deassert
    val ctrlTransferInsnInID  = Output(UInt(2.W))           // Control transfer instruction is decoded
    val ctrlTransferTargetMuxSel = Output(UInt(2.W))        // Jump target selection

    val mcounteren            = Input(UInt(32.W))           // Jump target selection
}

class Decoder()(p: IDParams) extends Module with RequireAsyncReset {
    val io = IO(new DecoderIO()(p))
}
