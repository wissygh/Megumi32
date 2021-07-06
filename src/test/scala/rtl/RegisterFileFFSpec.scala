/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-07-06
  * Description: Register file unit test.
  */
package megumi32.test

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util._
import megumi32.rtl._
import megumi32.include._
import scala.util.Random

class RegisterFileFFSpec extends FlatSpec with ChiselScalatestTester with Matchers {
    behavior of "RegisterFileFF"
    
    val p = new RegParams
    val maxValue: Int = math.pow(2, p.XLEN).toInt
    val regCount: Int = math.pow(2, p.ADDR_WIDTH).toInt

    def init(c: RegisterFileFF) {
        c.io.raddr foreach (_.poke(0.U))
        c.io.waddr foreach (_.poke(0.U))
        c.io.wdata foreach (_.poke(0.U))
        c.io.we    foreach (_.poke(false.B))

        c.clock.step(2)
    }

    def writeSinglePort(c: RegisterFileFF, din: Int, addr: Int) = {
        c.io.waddr(0).poke(addr.U)
        c.io.wdata(0).poke(din.U)
        c.io.we(0).poke(true.B)
        c.clock.step(1)
    }

    def read(c: RegisterFileFF, addr: List[Int]): Vec[UInt] = {
        require(addr.length <= p.READ_PORT_NUM, "Do not read above the read port number")
        addr.foreach(println(_))

        (0 until addr.length) foreach { x => 
            c.io.raddr(x).poke(addr(x).U)
        }
        c.io.rdata
    }

    it should "Write and Read all registers normally" in {
        test(new RegisterFileFF()(p)) { c => 
            val dataIn = Seq.fill(regCount)(Random.nextInt(maxValue))
            dataIn.zipWithIndex.foreach { case(d, idx) => 
                writeSinglePort(c, d, idx)
            }
            c.io.we(0).poke(false.B)
            c.clock.step(1)

            /** x0 is always 0 */
            val checkData = 0 :: dataIn.toList.tail
            
            /** Check feedbacks */
            c.io.raddr.zip(c.io.rdata) foreach { case(addr, data) => 
                (0 until dataIn.length) foreach { x => 
                    addr.poke(x.U)
                    data.expect(checkData(x).U)
                    c.clock.step(1)
                }
            }
        }
    }
}
