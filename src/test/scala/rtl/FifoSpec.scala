/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-24
  * Description: Unit tests for Fifo implementation: Fifo.scala
  */

package megumi32.test

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util._
import megumi32.include._
import megumi32.rtl.{Fifo, FifoIO}
import scala.collection.mutable.Queue
import scala.util.Random

class FifoSpec extends FlatSpec with ChiselScalatestTester with Matchers {
    behavior of "Fifo"

    val param = new FIFOParams()
    val maximum = param.DEPTH
    val maxValue = math.pow(2, param.DATA_WIDTH).toInt - 1

    /**
      * Finite queue model
      */
    def enqueueFinite[T](q: Queue[T], el: T, maximum: Int): Unit = {
        if(q.size < maximum) {
            q.enqueue(el)
        }
    }

    /** Initialize FIFO */
    def init(fifo: Fifo): Unit = {
        /** Initialize FIFO interface */
        fifo.io.fifoInf.fifo_push.poke(false.B)
        fifo.io.fifoInf.fifo_pop.poke(false.B)
        fifo.io.fifoInf.fifo_flush.poke(false.B)
        fifo.io.fifoInf.fifo_flush_but_first.poke(false.B)
        // fifo.reset.poke(false.B.asAsyncReset)

        /** Other initialization */
        fifo.io.testmode.poke(false.B)
        fifo.io.data_in.poke(0.U)

        fifo.clock.step(2)
        // fifo.reset.poke(true.B.asAsyncReset)
    }

    /** Enqueue hardware FIFO */
    def enq(el: Int, fifo: Fifo): Unit = {
        fifo.io.fifoInf.fifo_push.poke(true.B)
        fifo.io.data_in.poke(el.U)
        fifo.clock.step(1)
        fifo.io.fifoInf.fifo_push.poke(false.B)
    }

    /** Dequeue hardware FIFO */
    def deq(exp_el: Int, fifo: Fifo): Unit =  {
        fifo.io.fifoInf.fifo_pop.poke(true.B)
        fifo.io.data_out.expect(exp_el.U, s"Queue dequeue = ${fifo.io.data_out.peek().litValue}")
        fifo.clock.step(1)
        fifo.io.fifoInf.fifo_pop.poke(false.B)
    }

    it should "Normally push and pop" in {
        val ref_q = new Queue[Int]
        test(new Fifo()(param)) { c =>
            /** initialize */
            init(c)

            /** enqueue random elements */
            Array.fill(Random.nextInt(maximum-1)+1)(Random.nextInt(maxValue)) foreach { x => {
                    enqueueFinite(ref_q, x, maximum)
                    enq(x, c)
                }
            }

            /** dequeue test */
            for (i <- 0 until ref_q.size) {
                deq(ref_q.dequeue, c)
            }

            /** expect empty */
            c.io.fifoInf.fifo_empty.expect(true.B, s"Queue is empty: io.empty = ${c.io.fifoInf.fifo_empty.peek().litValue}")
        }
    }

    it should "Expect the overflow situation" in {
        val ref_q = new Queue[Int]
        test(new Fifo()(param)) { c => 
            /** initalize */
            init(c)

            /** Enqueue till full */
            Array.fill(param.DEPTH+1)(Random.nextInt(maxValue)) foreach { x => {
                    enqueueFinite(ref_q, x, maximum)
                    enq(x, c)
                }
            }

            /** Expect full */
            c.io.full.expect(true.B, s"Queue is full: io.full = ${c.io.full.peek().litValue}")

            /** dequeue test */
            for (i <- 0 until ref_q.size) {
                deq(ref_q.dequeue, c)
            }
        }
    }
}
