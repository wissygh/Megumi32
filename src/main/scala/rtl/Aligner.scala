/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-29
  * Description: Instruction Aligner
  */
package megumi32.rtl

import chisel3._
import chisel3.util._
import megumi32.include._
import chisel3.stage.ChiselStage

class AlignerIO()(p: IFParams) extends Bundle {
    val fetch_valid     = Input(Bool())
    val aligner_ready   = Output(Bool())        // Prevents overwriting the fetched instruction

    val if_valid        = Input(Bool())

    val fetch_rdata     = Input(UInt(p.XLEN.W))
    val instr_aligned   = Output(UInt(p.XLEN.W))
    val instr_valid     = Output(Bool())

    val branch_addr     = Input(UInt(p.XLEN.W))
    val branch          = Input(Bool())         // Asserted if we are branching/jumping now
}

class Aligner()(p: IFParams) extends Module with RequireAsyncReset{
    val io = IO(new AlignerIO()(p))

    /** FSM states */
    val aligned32 :: misaligned32 :: misaligned16 :: branch_misaligned :: Nil = Enum(4)
    val state = RegInit(aligned32)

    /** Local registers and wires */
    val r_instr_h       = RegInit(0.U((p.XLEN/2).W))
    val pc              = RegInit(0.U(p.XLEN.W))
    val aligner_ready   = RegInit(false.B)
    val update_state    = Wire(Bool())
    val pc_plus4 = pc + 4.U
    val pc_plus2 = pc + 2.U

    /**
      * Update the local registers using gate
      * The gate condition is given using update_state
      * You need to set the update_state first then call this func
      *
      * @param _pc: Bits -> next pc value
      * @param _state: Bits -> next state value
      */
    def update_reg(_pc: Bits, _state: Bits) {
        when(update_state) {
            pc := _pc
            state := _state
            r_instr_h := io.fetch_rdata(31, 16) // Always fetch higher 16 bits
            aligner_ready := io.aligner_ready
        }.otherwise {
            pc := pc
            state := state
            r_instr_h := r_instr_h
            aligner_ready := aligner_ready
        }
    }

    /** Default outputs */
    io.instr_valid      := io.fetch_valid
    io.instr_aligned    := io.fetch_rdata
    io.aligner_ready    := true.B
    update_state        := false.B

    /** FSM */
    switch(state) {
        is(aligned32) {
            when(io.fetch_rdata(1, 0) === 0x3.U(2.W)) {
                /**
                 * |    16    |    16    |
                 * | 32bit normal instr. |
                 * 
                 * Before we fetched a 32bit aligned instruction
                 * Therefore, noew the address is aligned too and it is 32 bits
                 * 
                 * All non compressed instructions have 00/10/01 in the lowest 2 bits
                 * Standard 32 bits instructions all have 11 in the lowest 2 bits
                 * 
                 * Generate gate condition first
                 * Gate id_valid with fetch_valid as the next state should be evaluated only if mem content is valid
                 */
                update_state := io.fetch_valid & io.if_valid
                io.instr_aligned := io.fetch_rdata
                update_reg(_pc = pc_plus4, _state = aligned32)
            }.otherwise {
                /**
                 * |    16    |    16    |
                 * |   ?[A]   |   RV32C  |
                 * 
                 * Before we fetched a 32bit aligned instruction
                 * Therefore, now the address is aligned too and it is 16bits
                 */
                update_state := io.fetch_valid & io.if_valid
                io.instr_aligned := io.fetch_rdata      // Only the first 16bit are used
                /**
                 * increase pc = pc + 2, now fetch:
                 * |    16    |    16    |
                 * |   ?[B]   |   ?[A]   |
                 */
                update_reg(_pc = pc_plus2, _state = misaligned32)
            }
        } // case: aligned32
        is(misaligned32) {
            /**
             * Now fetch:
             * |    16    |    16    |
             * |   ?[B]   |   ?[A]   |
             */
            when(r_instr_h(1, 0) === 0x3.U(2.W)) {
                /**
                  * Prev [A][1:0] === 2'b11
                  * Now Cat([B], [A]) = standard 32 bit instruction
                  * But the address is still misaligned: addr % 4 != 0 but addr %2 == 0
                  */
                update_state := io.fetch_valid & io.if_valid
                io.instr_aligned := Cat(io.fetch_rdata(15, 0), r_instr_h(15, 0))
                update_reg(_pc = pc_plus4, _state = misaligned32)
            }.otherwise {
                /**
                  * Prev [A][1:0] =/= 2'b11
                  * So [A] is a 16-bits compress instruction
                  * [B] remains unknown
                  * The instruction is 16bits misaligned
                  */
                update_state := io.if_valid // not need to gate id_valid with fetch_valid as the next state depends only on r_instr_h
                io.instr_aligned := Cat(io.fetch_rdata(31, 16), r_instr_h(15, 0))   // Only use the first 16 bits
                io.instr_valid := true.B
                /**
                  * We cannot overwrite the 32bit instruction just fetched
                  * so tell the IF stage to stall, the coming instruction goes to the FIFO
                  */
                io.aligner_ready := !io.fetch_valid
                update_reg(_pc = pc_plus2, _state = misaligned16)
            }
        } // case: misaligned32
        is(misaligned16) {
            /** This is 1 as we holded the value before with raw_instr_hold */
            io.instr_valid := !aligner_ready || io.fetch_valid
            when(io.fetch_rdata(1, 0) === 0x3.U(2.W)) {
                /**
                  * Fetch a 32 bit instruction
                  * 32 bit aligned
                  */
                update_state := (!aligner_ready | io.fetch_valid) & io.if_valid
                io.instr_aligned := io.fetch_rdata
                update_reg(_pc = pc_plus4, _state = aligned32)
            }.otherwise {
                /**
                  * Fetch a 16 bit compressed instruction
                  * 16bit aligned
                  */
                update_state := (!aligner_ready | io.fetch_valid) & io.if_valid
                io.instr_aligned := io.fetch_rdata      // Only the first 16 bits are used
                update_reg(_pc = pc_plus2, _state = misaligned32)
            }
        } // case: misaligned16
        is(branch_misaligned) {
            /** We jumped to a misaligned location, so now we received {TARGET, XXXX} */
            when(io.fetch_rdata(17, 16) === 0x3.U(2.W)) {
                /** 
                 * Misaligned 32bits instruction, stall and wait for the next 16bits.
                 * This is the worest situation, we need to stall 2 cycles
                 */
                update_state := io.fetch_valid & io.if_valid
                io.instr_valid := false.B
                io.instr_aligned := io.fetch_rdata
                update_reg(_pc = pc, _state = misaligned32)
            }.otherwise {
                update_state := io.fetch_valid & io.if_valid
                io.instr_aligned := Cat(Seq.fill(2)(io.fetch_rdata(31, 16)))
                update_reg(_pc = pc_plus2, _state = aligned32)
            }
        } // case: branch_misaligned
    }

    /** JUMP, BRANCH, SPEICAL JUMP control */
    when(io.branch) {
        update_state := true.B
        update_reg(_pc = io.branch_addr, _state = Mux(io.branch_addr(1), branch_misaligned, aligned32))
    }
}

object Aligner extends App {
    (new ChiselStage).emitVerilog(new Aligner()(new IFParams))
}
