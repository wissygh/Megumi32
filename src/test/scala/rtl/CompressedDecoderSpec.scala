/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-30
  * Description: Unit tests for Compressed Decoder implementation: CompressedDecoder.scala
  */
package megumi32.test

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util._
import megumi32.rtl.CompressedDecoder
import megumi32.include.IFParams

class CompressedDecoderSpec extends FlatSpec with ChiselScalatestTester with Matchers {
    behavior of "CompressedDecoder"

    it should "Convert all compressed instructions correctly" in {
        test(new CompressedDecoder()(new IFParams)) { c => 
            def feed(cin: UInt, cname: String) = {
                c.io.instr_in.poke(cin)
                println("%12s = 0x%08x".format(cname, c.io.instr_out.peek.litValue))
                c.clock.step(1)
            }
            /**
              * In this test, the correct output is :
              * C0:
                    c.addi4spn = 0x15810513
                    c.fld = 0x08863487
                    c.lw = 0x0506a583
                    c.flw = 0x05c72787
                    c.fsd = 0x02c53027
                    c.sw = 0x0684a623
                    c.fsw = 0x02d42a27
                C1:
                    c.addi = 0xfe420213
                    c.jal = 0xe6bff0ef
                    c.li = 0x00800493
                    c.addi16sp = 0x14010113
                    c.lui = 0xffff84b7
                    c.srli = 0x0195d593
                    c.srai = 0x41b6d693
                    c.andi = 0xfe547413
                    c.sub = 0x40d60633
                    c.xor = 0x00c4c4b3
                    c.or = 0x0087e7b3
                    c.and = 0x00b47433
                    c.j = 0x6960006f
                    c.beqz = 0x0e048963
                    c.bnez = 0xf0051ae3
                C2:
                    c.slli = 0x002b9b93
                    c.fldsp = 0x0b013387
                    c.lwsp = 0x0d812283
                    c.flwsp = 0x07012887
                    c.jr = 0x00020067
                    c.mv = 0x00e00333
                    c.ebreak = 0x00100073
                    c.jalr = 0x000400e7
                    c.add = 0x004484b3
                    c.fsdsp = 0x12613027
                    c.swsp = 0x06712623
                    c.fswsp = 0x0c812427
              */

            c.clock.step(2)

            /////////////////////////////////////////////
            // C0
            /////////////////////////////////////////////
            println("C0:")

            /** c.addi4spn */
            feed("b000_01010101_010_00".U, "c.addi4spn")

            /** c.fld */
            feed("b001_001_100_10_001_00".U, "c.fld")

            /** c.lw */
            feed("b010_010_101_01_011_00".U, "c.lw")

            /** c.flw */
            feed("b011_011_110_11_111_00".U, "c.flw")

            /** c.fsd */
            feed("b101_100_010_00_100_00".U, "c.fsd")

            /** c.sw */
            feed("b110_101_001_11_000_00".U, "c.sw")

            /** c.fsw */
            feed("b111_110_000_10_101_00".U, "c.fsw")

            /////////////////////////////////////////////
            // C1
            /////////////////////////////////////////////
            println("C1:")

            /** c.addi */
            feed("b000_1_00100_00100_01".U, "c.addi")

            /** c.jal */
            feed("b001_10101101011_01".U, "c.jal")
            
            /** c.li */
            feed("b010_0_01001_01000_01".U, "c.li")

            /** c.addi16sp */
            feed("b011_0_00010_01100_01".U, "c.addi16sp")

            /** c.lui */
            feed("b011_1_01001_11000_01".U, "c.lui")

            /** c.srli */
            feed("b100_0_00_011_11001_01".U, "c.srli")

            /** c.srai */
            feed("b100_0_01_101_11011_01".U, "c.srai")

            /** c.andi */
            feed("b100_1_10_000_00_101_01".U, "c.andi")

            /** c.sub */
            feed("b100_0_11_100_00_101_01".U, "c.sub")

            /** c.xor */
            feed("b100_0_11_001_01_100_01".U, "c.xor")

            /** c.or */
            feed("b100_0_11_111_10_000_01".U, "c.or")

            /** c.and */
            feed("b100_0_11_000_11_011_01".U, "c.and")

            /** c.j */
            feed("b101_01101010110_01".U, "c.j")

            /** c.beqz */
            feed("b110_010_001_11011_01".U, "c.beqz")

            /** c.bnez */
            feed("b111_110_010_00100_01".U, "c.bnez")

            /////////////////////////////////////////////
            // C2
            /////////////////////////////////////////////
            println("C2:")

            /** c.slli */
            feed("b000_0_10111_00010_10".U, "c.slli")

            /** c.fldsp */
            feed("b001_1_00111_10010_10".U, "c.fldsp")

            /** c.lwsp */
            feed("b010_0_00101_11011_10".U, "c.lwsp")

            /** c.flwsp */
            feed("b011_1_10001_10001_10".U, "c.flwsp")

            /** c.jr */
            feed("b100_0_00100_00000_10".U, "c.jr")

            /** c.mv */
            feed("b100_0_00110_01110_10".U, "c.mv")

            /** c.ebreak */
            feed("b100_1_00000_00000_10".U, "c.ebreak")

            /** c.jalr */
            feed("b100_1_01000_00000_10".U, "c.jalr")

            /** c.add */
            feed("b100_1_01001_00100_10".U, "c.add")

            /** c.fsdsp */
            feed("b101_100100_00110_10".U, "c.fsdsp")

            /** c.swsp */
            feed("b110_101101_00111_10".U, "c.swsp")

            /** c.fswsp */
            feed("b111_001011_01000_10".U, "c.fswsp")
        }
    }

    it should "Detect the illegal instructions" in {
        test(new CompressedDecoder()(new IFParams)) { c => 
            c.clock.step(2)

            def expIllegal(feed: UInt): Unit = {
                c.io.instr_in.poke(feed)
                c.io.illegal_instr.expect(true.B)
                c.clock.step(1)
            }

            /** zero */
            expIllegal(0.U)

            /** C0 Reserved */
            expIllegal(0x8000.U)

            /** c.srli / c.srai NSE */
            expIllegal(0x9001.U)
            expIllegal(0x9401.U)

            /** C1 Reserved */
            List(0x9c01.U, 0x9c41.U, 0x9c81.U, 0x9cc1.U) foreach { x => expIllegal(x)}

            /** c.slli NSE */
            expIllegal(0x1002.U)

            /** c.lwsp rd=0 RES */
            expIllegal(0x4002.U)

            /** c.jr rs1=0 RES */
            expIllegal(0x8002.U)
        }
    }
}
