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
import chisel3.stage.ChiselStage

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
    def illegal: Unit = { io.illegal_instr := true.B }

    io.illegal_instr := false.B
    out := 0.U

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
                is("b101".U) {
                    /** c.fsd -> fsd rs2', imm(rs1') */
                    if (FPU == 1)
                        out := Cat(0.U(4.W), in(6, 5), in(12), spgprs(in(4, 2)), spgprs(in(9, 7)), 3.U(3.W), in(11, 10), 0.U(3.W), Opcode.STORE_FP)
                    else illegal
                } // 101xxxxxxxxx00
                is("b110".U) {
                    /** c.sw -> sw rs2', imm(rs1') */
                    out := Cat(0.U(5.W), in(5), in(12), spgprs(in(4, 2)), spgprs(in(9, 7)), 2.U(3.W), in(11, 10), in(6), 0.U(2.W), Opcode.STORE)
                } // 110xxxxxxxxx00
                is("b111".U) {
                    if (FPU == 1)
                        out := Cat(0.U(5.W), in(5), in(12), spgprs(in(4, 2)), spgprs(in(9, 7)), 2.U(3.W), in(11, 10), in(6), 0.U(2.W), Opcode.STORE_FP)
                    else illegal
                } // 111xxxxxxxxx00
                is("b100".U) { illegal } // Reserved
            }
        }
        is("b01".U) {   // C1
            switch(funct3) {
                is("b000".U) {
                    /** c.addi -> addi rd, rd, nzimm / c.nop */
                    out := Cat(Fill(6, in(12)), in(12), in(6, 2), in(11, 7), 0.U(3.W), in(11, 7), Opcode.OPIMM)
                }
                is(List("b001".U, "b101".U)) {
                    /**
                      * 001: c.jal -> jal x1, imm
                      * 101: c.j   -> jal x0, imm
                      */
                    out := Cat(in(12), in(8), in(10, 9), in(6), in(7), in(2), in(11), in(5, 3), Fill(9, in(12)), 0.U(4.W), ~in(15), Opcode.JAL)
                }
                is("b010".U) {
                    when(in(11, 7) === 0.U(5.W)) {  // HINTs
                        /** HINT -> addi x0, x0, nzimm */
                        out := Cat(Fill(6, in(12)), in(12), in(6, 2), 0.U(5.W), 0.U(3.W), in(11, 7), Opcode.OPIMM)
                    }.otherwise {
                        /** c.li -> addi rd, x0, nzimm */
                        out := Cat(Fill(6, in(12)), in(12), in(6, 2), 0.U(5.W), 0.U(3.W), in(11, 7), Opcode.OPIMM)
                    }
                }
                is("b011".U) {
                    when(Cat(in(12), in(6, 2)) === 0.U(6.W)) { 
                        illegal
                    }.otherwise {
                        when(in(11, 7) === 2.U(5.W)) {
                            /** c.addi16sp -> addi x2, x2, nzimm */
                            out := Cat(Fill(3, in(12)), in(4, 3), in(5), in(2), in(6), 0.U(4.W), 2.U(5.W), 0.U(3.W), 2.U(5.W), Opcode.OPIMM)
                        }.elsewhen(in(11, 7) === 0.U(5.W)) {
                            /** HINT -> lui x0, nzimm */
                            out := Cat(Fill(15, in(12)), in(6, 2), in(11, 7), Opcode.LUI)
                        }.otherwise {
                            /** c.lui -> lui rd, imm */
                            out := Cat(Fill(15, in(12)), in(6, 2), in(11, 7), Opcode.LUI)
                        }
                    }
                }
                is("b100".U) {
                    switch(in(11, 10)) {
                        is(List("b00".U, "b01".U)) {
                            /**
                              * 00: c.srli -> srli rd', rd', shamt
                              * 01: c.srai -> srai rd', rd', shamt
                              */
                            when(in(12) === 1.U(1.W)) {
                                illegal
                            }.elsewhen(in(6, 2) === 0.U(5.W)) {
                                /** HINT */
                                out := Cat(0.U(1.W), in(10), 0.U(5.W), in(6, 2), spgprs(in(9, 7)), 5.U(3.W), spgprs(in(9, 7)), Opcode.OPIMM)
                            }.otherwise {
                                out := Cat(0.U(1.W), in(10), 0.U(5.W), in(6, 2), spgprs(in(9, 7)), 5.U(3.W), spgprs(in(9, 7)), Opcode.OPIMM)
                            }
                        }
                        is("b10".U) {
                            /** c.andi -> andi rd,  rd, imm */
                            out := Cat(Fill(6, in(12)), in(12), in(6, 2), spgprs(in(9, 7)), 7.U(3.W), spgprs(in(9, 7)), Opcode.OPIMM)
                        }
                        is("b11".U) {
                            switch(Cat(in(12), in(6, 5))) {
                                is("b000".U) {
                                    /** c.sub -> sub rd', rd', rs2' */
                                    out := Cat(1.U(2.W), 0.U(5.W), spgprs(in(4, 2)), spgprs(in(9, 7)), 0.U(3.W), spgprs(in(9, 7)), Opcode.OP)
                                }
                                is("b001".U) {
                                    /** c.xor -> xor rd', rd', rs2' */
                                    out := Cat(0.U(7.W), spgprs(in(4, 2)), spgprs(in(9, 7)), 4.U(3.W), spgprs(in(9, 7)), Opcode.OP)
                                }
                                is("b010".U) {
                                    /** c.or -> or rd', rd', rs2' */
                                    out := Cat(0.U(7.W), spgprs(in(4, 2)), spgprs(in(9, 7)), 6.U(3.W), spgprs(in(9, 7)), Opcode.OP)
                                }
                                is("b011".U) {
                                    /** c.and -> and rd', rd', rs2' */
                                    out := Cat(0.U(7.W), spgprs(in(4, 2)), spgprs(in(9, 7)), 7.U(3.W), spgprs(in(9, 7)), Opcode.OP)
                                }
                                is((4 until 8).map{ x => x.U }) { illegal }
                            }
                        }
                    }
                }
                is(List("b110".U, "b111".U)) {
                    /**
                      * 110: c.beqz -> beq rs1', x0, imm
                      * 111: c.bnqz -> bnq rs1', x0, imm
                      */
                    out := Cat(Fill(4, in(12)), in(6, 5), in(2), 0.U(5.W), spgprs(in(9, 7)), 0.U(2.W), in(13), in(11, 10), in(4, 3), in(12), Opcode.BRANCH)
                }
            }
        }
        is("b10".U) {   // C2
            switch(funct3) {
                is("b000".U) {
                    when(in(12) === 1.U(1.W)) {
                        illegal
                    }.otherwise {
                        when((in(6, 2) === 0.U(5.W)) || (in(11, 7) === 0.U(5.W))) {
                            /** HINT -> slli rd, rd, shamt */
                            out := Cat(0.U(7.W), in(6, 2), in(11, 7), 1.U(3.W), in(11, 7), Opcode.OPIMM)
                        }.otherwise {
                            /** c.slli -> slli rd, rd, shamt */
                            out := Cat(0.U(7.W), in(6, 2), in(11, 7), 1.U(3.W), in(11, 7), Opcode.OPIMM)
                        }
                    }
                }
                is("b001".U) {
                    /** c.fldsp -> fld rd, imm(x2) */
                    if(FPU == 1)
                        out := Cat(0.U(3.W), in(4, 2), in(12), in(6, 5), 0.U(3.W), 2.U(5.W), 3.U(3.W), in(11, 7), Opcode.LOAD_FP)
                    else illegal    
                }
                is("b010".U) {
                    /** c.lwsp -> lw rd, imm(x2) */
                    when(in(11, 7) === 0.U(5.W)) { illegal }
                    out := Cat(0.U(4.W), in(3, 2), in(12), in(6, 4), 0.U(2.W), 2.U(5.W), 2.U(3.W), in(11, 7), Opcode.LOAD)
                }
                is("b011".U) {
                    /** c.flwsp -> flw rd, imm(x2) */
                    if(FPU == 1)
                        out := Cat(0.U(4.W), in(3, 2), in(12), in(6, 4), 0.U(2.W), 2.U(5.W), 2.U(3.W), in(11, 7), Opcode.LOAD_FP)
                    else illegal
                }
                is("b100".U) {
                    when(in(12) === 0.U(1.W)) {
                        when(in(6, 2) === 0.U(5.W)) {
                            /** 
                              * c.jr -> jalr x0, rs1, 0 
                              * c.jr with rs1 = 0 is reserved
                              */
                            when(in(11, 7) === 0.U(5.W)) { illegal }
                            out := Cat(0.U(12.W), in(11, 7), 0.U(3.W), 0.U(5.W), Opcode.JALR)
                        }.otherwise {
                            when(in(11, 7) === 0.U(5.W)) {
                                /** HINT -> add x0, x0, rs2 */
                                out := Cat(0.U(7.W), in(6, 2), 0.U(5.W), 0.U(3.W), in(11, 7), Opcode.OP)
                            }.otherwise {
                                /** c.mv -> add rd, x0, rs2 */
                                out := Cat(0.U(7.W), in(6, 2), 0.U(5.W), 0.U(3.W), in(11, 7), Opcode.OP)
                            }
                        }
                    }.otherwise {
                        when(in(6, 2) === 0.U(5.W)) {
                            when(in(11, 7) === 0.U(5.W)) {
                                /** c.ebreak -> eberak */
                                out := 0x00100073.U(32.W)
                            }.otherwise {
                                /** c.jalr -> jalr x1, rs1, 0 */
                                out := Cat(0.U(12.W), in(11, 7), 0.U(3.W), 1.U(5.W), Opcode.JALR)
                            }
                        }.otherwise {
                            when(in(11, 7) === 0.U(5.W)) {
                                /** HINT -> add x0, x0, rs2 */
                                out := Cat(0.U(7.W), in(6, 2), in(11, 7), 0.U(3.W), in(11, 7), Opcode.OP)
                            }.otherwise {
                                /** c.add -> add rd, rd, rs2 */
                                out := Cat(0.U(7.W), in(6, 2), in(11, 7), 0.U(3.W), in(11, 7), Opcode.OP)
                            }
                        }
                    }
                }
                is("b101".U) {
                    /** c.fsdsp -> fsd rs2, imm(x2) */
                    if(FPU == 1) //instr_i[12:10] -> offset[5:3], instr_i[9:7] -> offset[8:6]
                        out := Cat(0.U(3.W), in(9, 7), in(12), in(6, 2), 2.U(5.W), 3.U(3.W), in(11, 10), 0.U(3.W), Opcode.STORE_FP)
                    else illegal
                }
                is("b110".U) {
                    /** c.swsp -> sw rs2, imm(x2) */
                    out := Cat(0.U(4.W), in(8, 7), in(12), in(6, 2), 2.U(5.W), 2.U(3.W), in(11, 9), 0.U(2.W), Opcode.STORE)
                }
                is("b111".U) {
                    /** c.fswsp -> fsw rs2, imm(x2) */
                    if(FPU == 1)
                        out := Cat(0.U(4.W), in(8, 7), in(12), in(6, 2), 2.U(5.W), 2.U(3.W), in(11, 9), 0.U(2.W), Opcode.STORE_FP)
                    else illegal
                }
            }
        }
        is("b11".U) { io.instr_out := io.instr_in }
    }
    io.is_compressed := (io.instr_in(1, 0) =/= 0x3.U(2.W))
}

object CompressedDecoder extends App {
    (new ChiselStage).emitVerilog(new CompressedDecoder(FPU = 1)(new IFParams))
}
