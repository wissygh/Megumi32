/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-25
  * Description: Open Bus Interface adapter. Translates transaction request 
  *              on the trans_* interface into an OBI A channel transfer.   
  *              The OBI R channel transfer translated (i.e. passed on) as  
  *              a transaction response on the resp_* interface.            
  *                                                                         
  *              This adapter does not limit the number of outstanding      
  *              OBI transactions in any way. 
  */
package megumi32.rtl

import chisel3._
import chisel3.util._
import megumi32.include._
import chisel3.stage.ChiselStage

class ObiInterfaceIO()(p: ObiParams) extends Bundle {
    /** 
     * Transaction interface (S)
     * Request from prefetch controller
     */
    val transInf = Flipped(new TransactionExInf()(p))

    /** 
     * OBI interface (M)
     * Send request to Instruction Cache
     */
    val obiInf = new ObiInf()(p)
}

class ObiInterface()(p: ObiParams) extends Module with RequireAsyncReset {
    val io = IO(new ObiInterfaceIO()(p))

    /**
      * Helper function to directly pass request onto the obi interface
      */
    def dirPassRequest: ObiInterfaceIO => Unit = { io =>
        io.obiInf.req   := io.transInf.transRequest.valid
        io.obiInf.addr  := io.transInf.transRequest.bits.addr
        io.obiInf.we    := io.transInf.transRequest_we
        io.obiInf.be    := io.transInf.transRequest_be
        io.obiInf.wdata := io.transInf.transRequest_wdata
        io.obiInf.atop  := io.transInf.transRequest_atop
    }

    /** FSM definition */
    val transparent :: registered :: Nil = Enum(2)
    val state = RegInit(transparent)

    //////////////////////////////////////////////////////////////////////////////
    // OBI R Channel
    //////////////////////////////////////////////////////////////////////////////

    /**
      * The OBI R channel signals are passed on directly on the transaction response
      * interface (transResp_*). It is assumed that the consumer of the transaction response
      * is always receptive when transResp_valid = 1 (otherwise a response would get dropped)
      * 
      * That is: the rready signal in R channel of OBI protocol v1.0 is missing in our
      * definition.
      */
    io.transInf.transResp_valid := io.obiInf.rvalid
    io.transInf.transResp_rdata := io.obiInf.rdata
    io.transInf.transResp_err   := io.obiInf.err

    //////////////////////////////////////////////////////////////////////////////
    // OBI A Channel
    //////////////////////////////////////////////////////////////////////////////

    if (p.TRANS_STABLE == 1) {
        /**
          * If the incoming transaction itself is stable, then it satisfies the OBI protocol
          * and signals can be passed to/from OBI directly
          */
        dirPassRequest(io)
        io.transInf.transRequest.ready := io.obiInf.gnt

        /** FSM not used */
        state := transparent
    } else {
        /**
          * If the incoming transaction itself is not stable; use an FSM to make sure that
          * the OBI address phase signals are kept stable during non-granted requests.
          */
        
        /** OBI A channel regsiters (to keep A channel stable) */
        val aRegs = RegInit(0.U.asTypeOf(new Bundle{
                val addr    = UInt(p.XLEN.W)
                val we      = Bool()
                val be      = UInt(p.BE_WIDTH.W)
                val wdata   = UInt(p.XLEN.W)
                val atop    = UInt(p.ATOP_WDITH.W)
            })
        )

        //////////////////////////////////////////////////////////////////////////////
        // OBI FSM
        //////////////////////////////////////////////////////////////////////////////

        /** FSM to control OBI A channel signals */
        switch(state) {
            is(transparent) {
                /** Default state. Transaction requests are passed directly on to the OBI A channel */
                when(io.obiInf.req && !io.obiInf.gnt) {
                    /**
                      * OBI request not immediately granted. Move to "registered" state such that OBI address phase
                      * signals can be kept stable while the transaction request (trans_*) can possibly change.
                      */
                    state := registered

                    /** Keep OBI A channel signals stable throughout "registered" state */
                    aRegs.addr    := io.obiInf.addr
                    aRegs.we      := io.obiInf.we
                    aRegs.be      := io.obiInf.be
                    aRegs.wdata   := io.obiInf.wdata
                    aRegs.atop    := io.obiInf.atop   
                }
            } // case transparent
            is(registered) {
                /** Registered state. OBI address phase signals are kept stable (driven from registers) */
                when(io.obiInf.gnt) {
                    /** Received grant. Move back to "transparent" state such that next transaction request can be pass on */
                    state := transparent
                }
            } // case registered
        }

        /** Combinational connections */
        when(state === transparent) {
            dirPassRequest(io)
        }.otherwise {
            /** state === registered */
            io.obiInf.req   := true.B
            io.obiInf.addr  := aRegs.addr
            io.obiInf.we    := aRegs.we
            io.obiInf.be    := aRegs.be
            io.obiInf.wdata := aRegs.wdata
            io.obiInf.atop  := aRegs.atop
        }

        /**
          * Always ready to accept a new transfer requests when previous A channel
          * transfer has been granted. Note that our obi interface does not limit
          * the number of outstanding transactions in any way.
          */
        io.transInf.transRequest.ready := (state === transparent)
    }
}

object ObiInterface extends App {
    (new ChiselStage).emitVerilog(new ObiInterface()(new ObiParams))
}
