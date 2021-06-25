/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-25
  * Description: Prefetch Buffer that caches instructions. this cuts overly
  *              long veritical paths to the instruction cache.
  */
package megumi32.rtl

import chisel3._
import chisel3.util._
import megumi32.include._
import chisel3.stage.ChiselStage

class PrefetchBufferIO()(p: IFParams) extends Bundle {
    /** Fetch stage interface */
    val fsInf = new FetchStageInf()(p)
    val fetch_rdata = DecoupledIO(UInt(p.XLEN.W))

    /** Instruction memory/cache interface */
    val instrInf = new InstrInf()(p)
}

class PrefetchBuffer()(p: IFParams) extends Module with RequireAsyncReset {
    val io = IO(new PrefetchBufferIO()(p))

    /**
      * FIFO_DEPTH controls also the number of outstanding memory requests:
      * - FIFO_DEPTH > 1      -> respect assertion in prefetch controller
      * - FIFO_DEPTH % 2 == 0 -> FIFO implementation limits
      */
    val FIFO_DEPTH: Int = 2
    val FIFO_ADDR_DEPTH: Int = log2Ceil(FIFO_DEPTH)

    val fifoRdata, respRdata = Wire(UInt(p.XLEN.W))

    /** Submodule initialize */
    val sp = new IFParams(Some(FIFO_DEPTH))
    val prefetchController = Module(new PrefetchController()(sp))
    val fifo = Module(new Fifo()(new FIFOParams(Some(FIFO_DEPTH))))
    val obiInterface = Module(new ObiInterface()(new ObiParams(Some(FIFO_DEPTH))))

    /** Connections between modules */

    /** Prefetch Controller */
    prefetchController.io.fsInf <> io.fsInf
    prefetchController.io.transInf.transRequest <> obiInterface.io.transInf.transRequest
    prefetchController.io.transInf.transResp_valid := obiInterface.io.transInf.transResp_valid
    prefetchController.io.fifoInf <> fifo.io.fifoInf
    prefetchController.io.full := fifo.io.full

    prefetchController.io.fetch_ready := io.fetch_rdata.ready
    io.fetch_rdata.valid := prefetchController.io.fetch_valid

    /** FIFO */
    fifo.io.testmode := false.B
    fifo.io.data_in := respRdata
    fifoRdata := fifo.io.data_out

    /** 
     * First POP from the FIFO if it is not empty
     * Otherwise, try to fall-through it
     */
    io.fetch_rdata.bits := Mux(fifo.io.fifoInf.fifo_empty, respRdata, fifoRdata)

    /** OBI interface */
    obiInterface.io.transInf.transRequest.bits.addr := 
        Cat(prefetchController.io.transInf.transRequest.bits.addr(31, 2), 0.U(2.W))
    obiInterface.io.transInf.transRequest_we := false.B     // Never write
    obiInterface.io.transInf.transRequest_be := 0xF.U(4.W)
    obiInterface.io.transInf.transRequest_wdata := 0.U
    obiInterface.io.transInf.transRequest_atop := 0.U

    respRdata := obiInterface.io.transInf.transResp_rdata
    
    io.instrInf.req := obiInterface.io.obiInf.req
    obiInterface.io.obiInf.gnt := io.instrInf.gnt
    io.instrInf.addr := obiInterface.io.obiInf.addr
    obiInterface.io.obiInf.rdata := io.instrInf.rdata
    obiInterface.io.obiInf.rvalid := io.instrInf.rvalid

    obiInterface.io.obiInf.err := false.B
}

object PrefetchBuffer extends App {
    (new ChiselStage).emitVerilog(new PrefetchBuffer()(new IFParams))
}
