/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-07-06
  * Description: Register file with 31x 32 bit registers. Register 0
  *              is fiexd to 0. This register file is based on filp-
  *              flops. Also supports the fp-regsiter file now if FPU
  *              is 1, floating point operations take values from the
  *              X register file.
  */
package megumi32.rtl

import chisel3._
import chisel3.util._
import megumi32.include._
import chisel3.stage.ChiselStage

class RegisterFileFFIO()(p: RegParams) extends Bundle {
    require(p.READ_PORT_NUM >= 1, "Read port number must large or equal than 1")
    require(p.WRITE_PORT_NUM >= 1, "Write port number must large or equal than 1")

    // val scan_cg_en_i = Input(Bool()) // no use in this implementation

    /** Read ports */
    val raddr = Input(Vec(p.READ_PORT_NUM, UInt(p.ADDR_WIDTH.W)))
    val rdata = Output(Vec(p.READ_PORT_NUM, UInt(p.XLEN.W)))

    /** Write ports */
    val waddr = Input(Vec(p.WRITE_PORT_NUM, UInt(p.ADDR_WIDTH.W)))
    val wdata = Input(Vec(p.WRITE_PORT_NUM, UInt(p.XLEN.W)))
    val we    = Input(Vec(p.WRITE_PORT_NUM, Bool()))
}

class RegisterFileFF()(p: RegParams) extends Module with RequireAsyncReset {
    val io = IO(new RegisterFileFFIO()(p))

    /** Local parameters */
    val NUM_WORDS: Int      = math.pow(2, p.ADDR_WIDTH-1).toInt
    val NUM_FP_WORDS: Int   = NUM_WORDS
    val NUM_TOT_WORDS: Int  = if (p.FPU==1) NUM_WORDS + NUM_FP_WORDS else NUM_WORDS

    /** Integer register file */
    val mem = Mem(NUM_WORDS, UInt(p.XLEN.W))

    /** FP register file */
    val mem_fp = Mem(NUM_FP_WORDS, UInt(p.XLEN.W))

    /** Masked write addresses */
    val waddr = Wire(Vec(p.WRITE_PORT_NUM, UInt(p.ADDR_WIDTH.W)))
    
    /** Write enable signals for all registers */
    val we_dec = Wire(Vec(p.WRITE_PORT_NUM, Vec(NUM_TOT_WORDS, Bool())))

    /**
      * -- READ : Read address decoder RAD
      */
    if (p.FPU == 1) {
        io.rdata.zipWithIndex.foreach { case(data, idx) =>
            data := Mux(io.raddr(idx)(5), mem_fp(io.raddr(idx)(4, 0)), mem(io.raddr(idx)(4, 0)))
        }
    } else {
        io.rdata.zipWithIndex.foreach { case(data, idx) =>
            data := mem(io.raddr(idx)(4, 0))
        }
    }

    /**
      * -- WRITE : Write Address Decoder (WAD), combinatorial process
      */
    waddr.zipWithIndex.foreach { case(addr, idx) => 
        addr := io.waddr(idx)
    }

    (0 until NUM_TOT_WORDS) foreach { gidx => 
        we_dec.zipWithIndex.foreach { case(we, idx) =>
            we(gidx) := Mux(waddr(idx) === gidx.U, io.we(idx), false.B)
        }
    }

    /**
      * -- WRITE : Write operation
      */
    /** R0 is nil */
    mem(0) := 0.U

    (1 until NUM_WORDS) foreach { i =>
        when(!(Seq.tabulate(p.WRITE_PORT_NUM)(x => !we_dec(x)(i)).reduce((x, y) => x && y))) {
            mem(i) := PriorityMux(Seq.tabulate(p.WRITE_PORT_NUM)(x => (we_dec(x)(i) -> io.wdata(x))).reverse)
        } 
    }

    if (p.FPU == 1) {
        /** Floating point registers */
        (0 until NUM_FP_WORDS) foreach { l =>
            when(!Seq.tabulate(p.WRITE_PORT_NUM)(x => !we_dec(x)(l+NUM_WORDS)).reduce((x, y) => x && y)) {
                mem_fp(l) := PriorityMux(Seq.tabulate(p.WRITE_PORT_NUM)(x => (we_dec(x)(l+NUM_WORDS) -> io.wdata(x))).reverse)
            } 
        }
    }
}

object RegisterFileFF extends App {
    (new ChiselStage).emitVerilog(new RegisterFileFF()(new RegParams))
}
