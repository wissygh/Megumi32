/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-24
  * Description: Defines fifo for prefetch buffer
  */
package megumi32.rtl

import chisel3._
import chisel3.util._
import megumi32.include._
import chisel3.stage.ChiselStage

class FifoIO()(p: FIFOParams) extends Bundle {
    require(p.DEPTH > 0, "DEPTH must be greater than 0.")

    /** FIFO interface for FIFO, adding extra IO ports full */
    val fifoInf     = Flipped(new FIFOInf()(p))
    val full        = Output(Bool())

    val testmode    = Input(Bool())
    
    /** Data in/out the queue */
    val data_in     = Input(UInt(p.DATA_WIDTH.W))
    val data_out    = Output(UInt(p.DATA_WIDTH.W))

    // debug
    val mem_out = Output(Vec(p.DEPTH, UInt(p.DATA_WIDTH.W)))
    val wp = Output(UInt(p.ADDR_DEPTH.W))
    val rp = Output(UInt(p.ADDR_DEPTH.W))
}

class Fifo()(p: FIFOParams) extends Module with RequireAsyncReset {
    val io = IO(new FifoIO()(p))

    /** Local parameters */
    /** FIFO depth - handle the case of pass-through, synthesizer will do constant propagation */
    val FIFO_DEPTH: Int = if (p.DEPTH > 0) p.DEPTH else 1
    /** Clock gating control */
    val gate_clock = Wire(Bool())
    /** Pointer to the read and write section of the queue */
    /** We use chisel built-in counter here */
    val read_pointer, write_pointer = Counter(math.pow(2, p.ADDR_DEPTH).toInt)
    /** Keep a counter to keep track of the current queue status */
    val status_cnt = RegInit(0.U((p.ADDR_DEPTH+1).W))   // this integer will be truncated by the synthesis tool
    /** Actual memory */
    val mem = Mem(FIFO_DEPTH, UInt(p.DATA_WIDTH.W))     // In this case we choose not to use q/n model

    io.fifoInf.fifo_cnt := status_cnt

    /** Status flags */
    if (p.DEPTH == 0) {
        io.fifoInf.fifo_empty := ~io.fifoInf.fifo_push
        io.full               := ~io.fifoInf.fifo_pop
    } else {
        io.fifoInf.fifo_empty := (status_cnt === 0.U) && !(p.FALL_THROUGH & io.fifoInf.fifo_push)
        io.full               := status_cnt === (FIFO_DEPTH.U)(p.ADDR_DEPTH, 0)
    }

    /** Read and write queue logic */
    /** Default assignment */
    gate_clock := true.B

    if(p.DEPTH == 0) io.data_out := io.data_in
    else             io.data_out := mem(read_pointer.value)

    /** Push a new element to the queue */
    when(io.fifoInf.fifo_push && ~io.full) {
        /** Push the data onto the queue */
        mem(write_pointer.value) := io.data_in
        /** Un-gate the clock, we want to write something */
        gate_clock := false.B
        /** Increment the write counter */
        write_pointer.inc()
        /** Increment the overall count */
        status_cnt := status_cnt + 1.U
    }

    /** Pop a element from the queue */
    when(io.fifoInf.fifo_pop && ~io.fifoInf.fifo_empty) {
        /** Read from the queue is a default assignment */
        /** but increment the read pointer */
        read_pointer.inc()
        /** Decrement the overall count */
        status_cnt := status_cnt - 1.U
    }
    
    /** Keep the counter pointer stable if we push and pop at the same time */
    when(io.fifoInf.fifo_push && io.fifoInf.fifo_pop && ~io.full && ~io.fifoInf.fifo_empty) {
        status_cnt := status_cnt
    }

    /** FIFO is in pass through mode -> do not change pointers */
    when(p.FALL_THROUGH.asBool && (status_cnt === 0.U) && io.fifoInf.fifo_push) {
        io.data_out := io.data_in
        when(io.fifoInf.fifo_pop) {
            /** Remain counters unchange */
            status_cnt := status_cnt
            read_pointer.value := read_pointer.value
            write_pointer.value := write_pointer.value
        }
    }

    /** Sequential process */
    switch(Cat(io.fifoInf.fifo_flush_but_first, io.fifoInf.fifo_flush)) {
        is(1.U(2.W)) {
            /** Flush the FIFO */
            read_pointer.reset()
            write_pointer.reset()
            status_cnt := 0.U
        }
        is(2.U(2.W)) {
            /** Flush the FIFO but keep the first instruction alive if present */
            read_pointer.value := Mux(status_cnt > 0.U, read_pointer.value, 0.U)
            write_pointer.value := Mux(status_cnt > 0.U, read_pointer.value + 1.U, 0.U)
            status_cnt := Mux(status_cnt > 0.U, 1.U, 0.U)
        }
    }

    // Debug
    io.mem_out.zipWithIndex foreach {
        case (x, idx) => x := mem(idx)
    }
    io.wp := write_pointer.value
    io.rp := read_pointer.value
}

object Fifo extends App {
    (new ChiselStage).emitVerilog(new Fifo()(new FIFOParams))
}
