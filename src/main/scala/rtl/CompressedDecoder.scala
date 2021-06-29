/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-29
  * Description: Decodes RISC-V compressed instructions into their RV32
  *              equivalent. This module is fully combinatorial.
  */
package megumi32.rtl

import chisel3._
import chisel3.util._
import megumi32.include._

/**
  * We choose to put the IO definition and class definition together because
  * the IO is simple and would never changed
  *
  * @param FPU: Int -> Is the processor implement the FPU
  * @param p: IFParams -> IF stage parameters
  */
class CompressedDecoder(FPU: Int = 0)(p: IFParams) extends Module with RequireAsyncReset {
    val io = IO(new Bundle {
        val instr_in      = Input(UInt(p.XLEN.W))
        val instr_out     = Output(UInt(p.XLEN.W))
        val is_compressed = Output(Bool())
        val illegal_instr = Output(Bool())
    })

    /** 
     * Decode table. We use SV/verilog-like style to do so cause
     * from compressed instructin -> 32bit instruction has many sepcial cases
     */
    val funct3 = io.instr_in(15, 13)
    val in = io.instr_in
    val out = io.instr_out
    val spgprs: Bits => Bits = x => Cat(1.U(2.W), x)
    val illegal: Unit = { io.illegal_instr := true.B }

    switch(io.instr_in(1, 0)) {
        is("b00".U) {   // C0
            switch(funct3) {
                is("b000".U) {
                    /** c.addi4spn -> addi rd', sp, 4*imm */
                    out := Cat(0.U(2.W), in(10, 7), in(12, 11), in(5), in(6), 0.U(2.W), 2.U(5.W), 0.U(3.W), spgprs(in(4, 2)), Opcode.OPIMM)
                    when(in(12, 5) === 0.U(8.W)) { illegal }
                } // 000xxxxxxxxx00
                is("b001".U) {
                    /** c.fld -> fld rd', imm(rs1') */
                    if (FPU == 1)
                        out := Cat(0.U(4.W), in(6, 5), in(12, 10), 0.U(3.W), spgprs(in(9, 7)), 3.U(3.W), spgprs(in(4, 2)), Opcode.LOAD_FP)
                    else illegal
                } // 001xxxxxxxxx00
                is("b010".U) {
                    /** c.lw -> lw rd', imm(rs1') */
                    out := Cat(0.U(5.W), in(5), in(12, 10), in(6), 0.U(2.W), spgprs(in(9, 7)), 2.U(3.W), spgprs(in(4, 2)), Opcode.LOAD)
                } // 010xxxxxxxxx00
                is("b011".U) {
                    /** c.flw -> flw rd', imm(rs1') */
                    if (FPU == 1)
                        out := Cat(0.U(5.W), in(5), in(12, 10), in(6), 0.U(2.W), spgprs(in(9, 7)), 2.U(3.W), spgprs(in(4, 2)), Opcode.LOAD_FP)
                    else illegal
                } // 011xxxxxxxxx00
                is("101".U) {
                    /** c.fsd -> fsd rs2', imm(rs1') */
                    if (FPU == 1)
                        out := Cat(0.U(4.W), in(6, 5), in(12), spgprs(in(4, 2)), spgprs(in(9, 7)), 3.U(3.W), in(11, 10), 0.U(3.W), Opcode.STORE_FP)
                    else illegal
                } // 101xxxxxxxxx00
                is("110".U) {
                    /** c.sw -> sw rs2', imm(rs1') */
                    out := Cat(0.U(5.W), in(5), in(12), spgprs(in(4, 2)), spgprs(in(9, 7)), 2.U(3.W), in(11, 10), in(6), 0.U(2.W), Opcode.STORE)
                } // 110xxxxxxxxx00
                is("111".U) {
                    if (FPU == 1)
                        out := Cat(0.U(5.W), in(5), in(12), spgprs(in(4, 2)), spgprs(in(9, 7)), 2.U(3.W), in(11, 10), in(6), 0.U(2.W), Opcode.STORE_FP)
                    else illegal
                } // 111xxxxxxxxx00
                is("100".U) { illegal }
            }
        }
        is("b01".U) {   // C1
            switch(funct3) {

            }
        }
        is("b10".U) {   // C2
            switch(funct3) {

            }
        }
        is("b11".U) { io.instr_out := io.instr_in }
    }
    io.is_compressed := (io.instr_in(1, 0) =/= 0x3.U(2.W))
}
