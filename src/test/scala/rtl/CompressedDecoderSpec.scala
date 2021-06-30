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
        test(new CompressedDecoder(FPU=1)(new IFParams)) { c => 
            def feed(cin: UInt) = {
                c.io.instr_in.poke(cin)
                println(s"out = ${c.io.instr_out.peek.litValue.toString(radix = 16)}")
                c.clock.step(1)
            }

            c.clock.step(2)

            /////////////////////////////////////////////
            // C0
            /////////////////////////////////////////////
            println("C0:")

            /** c.addi4spn */
            feed("b000_01010101_010_00".U)

            /** c.fld */
            feed("b001_001_100_10_001_00".U)

            /** c.lw */
            feed("b010_010_101_01_011_00".U)

            /** c.flw */
            feed("b011_011_110_11_111_00".U)

            /** c.fsd */
            feed("b101_100_010_00_100_00".U)

            /** c.sw */
            feed("b110_101_001_11_000_00".U)

            /** c.fsw */
            feed("b111_110_000_10_101_00".U)

            /////////////////////////////////////////////
            // C1
            /////////////////////////////////////////////
            println("C1:")

            /** c.addi */
            feed("b000_1_00100_00100_01".U)

            /** c.jal */
            feed("b001_10101101011_01".U)
            
            /** c.li */
            feed("b010_0_01001_01000_01".U)

            /** c.addi16sp */
            feed("b011_0_00010_01100_01".U)

            /** c.lui */
            feed("b011_1_01001_11000_01".U)

            /** c.srli */
            feed("b100_0_00_011_11001_01".U)

            /** c.srai */
            feed("b100_0_01_101_11011_01".U)

            /** c.andi */
            feed("b100_1_10_000_00_101_01".U)

            /** c.sub */
            feed("b100_0_11_100_00_101_01".U)

            /** c.xor */
            feed("b100_0_11_001_01_100_01".U)

            /** c.or */
            feed("b100_0_11_111_10_000_01".U)

            /** c.and */
            feed("b100_0_11_000_11_011_01".U)

            /** c.j */
            feed("b101_01101010110_01".U)

            /** c.beqz */
            feed("b110_010_001_11011_01".U)

            /** c.bnez */
            feed("b111_110_010_00100_01".U)

            /////////////////////////////////////////////
            // C2
            /////////////////////////////////////////////
            println("C2:")

            /** c.slli */
            feed("b000_0_10111_00010_10".U)

            /** c.fldsp */
            feed("b001_1_00111_10010_10".U)

            /** c.lwsp */
            feed("b010_0_00101_11011_10".U)

            /** c.flwsp */
            feed("b011_1_10001_10001_10".U)

            /** c.jr */
            feed("b100_0_00100_00000_10".U)

            /** c.mv */
            feed("b100_0_00110_01110_10".U)

            /** c.ebreak */
            feed("b100_1_00000_00000_10".U)

            /** c.jalr */
            feed("b100_1_01000_00000_10".U)

            /** c.add */
            feed("b100_1_01001_00100_10".U)

            /** c.fsdsp */
            feed("b101_100100_00110_10".U)

            /** c.swsp */
            feed("b110_101101_00111_10".U)

            /** c.fswsp */
            feed("b111_001011_01000_10".U)
        }
    }
}
