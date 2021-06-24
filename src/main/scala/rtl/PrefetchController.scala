/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-23
  * Description: Prefetch Controller which receives control flow            
                 information (req_i, branch_*) from the Fetch stage         
                 and based on that performs transactions requests to the    
                 bus interface adapter instructions. Prefetching based on   
                 incrementing addressed is performed when no new control    
                 flow change is requested. New transaction requests are     
                 only performed if it can be guaranteed that the fetch FIFO 
                 will not overflow (resulting in a maximum of DEPTH         
                 outstanding transactions.
  */

package megumi32.rtl

import chisel3._
import chisel3.util._
import megumi32.include._
import chisel3.stage.ChiselStage

class PrefetchControllerIO()(p: IFParams) extends Bundle {
    /** asyncReset */
    // val asyncReset = Input(AsyncReset())

    /** Fetch stage interface */
    val fsInf = new FetchStageInf()(p)

    /** Transaction request and response interface */
    val transInf = new TransactionInf()(p)

    /** Fetch interface is ready/valid */
    val fetch_ready = Input(Bool())
    val fetch_valid = Output(Bool())

    /** FIFO interface */
    val fifoInf = new FIFOInf()(p)
}

class PrefetchController()(p: IFParams) extends Module with RequireAsyncReset{
    val io = IO(new PrefetchControllerIO()(p))

    /**
      * All implementations are in negative asyncReset domain
      * in order to implement the follow segmentations:
      * 
      *     always @(posedge clock or posedge io_asyncReset) begin
      *         // ...
      * 
      */
      
    /** FSM state definition */
    val idle :: branchWait :: Nil = Enum(2)
    val state = RegInit(idle)
    
    val cnt = RegInit(0.U((p.FIFO_ADDR_DEPTH+1).W))    // Transaction counter
    val count_up, count_down = Wire(Bool())                 // In/Decrement outstanding transactions
    
    val flush_cnt = RegInit(0.U((p.FIFO_ADDR_DEPTH+1).W))   // Response flush counter (to flush speculative responses after branch)

    /** Transaction address */
    val trans_addr_q = RegInit(0.U(32.W))
    val trans_addr_incr = Wire(UInt(32.W))

    /** Word-aligned branch target address */
    val aligned_branch_addr = Wire(UInt(32.W))
    
    /** FIFO auxiliary signal */
    val fifo_valid = Wire(Bool())           // FIFO output valid (if !fifo_empty)
    val fifo_cnt_masked = Wire(Bool())      // FIFO_cnt signal, masked when we are branching to allow a new memory request in that cycle

    //////////////////////////////////////////////////////////////////////////////
    // Prefetch buffer status
    //////////////////////////////////////////////////////////////////////////////

    /** Busy is there are ongoing (or potentially outstanding) transfers */
    io.fsInf.busy := (cnt =/= 0.U(3.W)) || io.transInf.transRespValid

    //////////////////////////////////////////////////////////////////////////////
    // IF/ID interface
    //////////////////////////////////////////////////////////////////////////////
    
    /**
         * Fetch valid control. Fetch never valid if jumping or flushing responses.
         * Fetch valid if there are instructions in FIFO or there is an incoming
         * instruction from memory.
         */
    io.fetch_valid := (fifo_valid || io.transInf.transRespValid) && !(io.fsInf.branch || (flush_cnt > 0.U))

    //////////////////////////////////////////////////////////////////////////////
    // Transaction request generation
    //
    // Assumes that corresponding response is at least 1 cycle after request
    //
    // - Only request transaction when fetch stage requires fetch (io.fsInf.req), and
    // - make sure that FIFO never overflows (io.fifoInf.fifo_cnt + cnt.q < DEPTH)
    //////////////////////////////////////////////////////////////////////////////

    /** Prefetcher will only perform word fetches */
    aligned_branch_addr := Cat(io.fsInf.branch_addr(31, 2), 0.U(2.W))

    /** Increment address (always word fetch) */
    trans_addr_incr := Cat(trans_addr_q(31, 2), 0.U(2.W)) + 4.U(32.W)

    /** Transaction request generation */
    if (p.PULP_OBI == 0) {
        /**
             * OBI compatible (avoids combinatorial path from instr_rvalid to instr_req).
             * MULTIPLE trans_* transactions can be ISSUED (and accepted) before a response
             * (resp_*) is received.
             * 
             */
        io.transInf.transRequest.valid := io.fsInf.req && (fifo_cnt_masked + cnt < p.DEPTH.U)
    } else {
        /**
             * Legacy PULP OBI behavior, i.e. only issue subsequent transaction if preceding transfer
             * is about to finish (re-introducint timing critial path from instr_rvalid to instr_req)
             */
        io.transInf.transRequest.valid := Mux(cnt === 0.U(3.W), 
                                                io.fsInf.req && (fifo_cnt_masked + cnt < p.DEPTH.U),
                                                io.fsInf.req && (fifo_cnt_masked + cnt < p.DEPTH.U) && io.transInf.transRespValid)
    }

    /**
         * Optimization:
         * fifo_cnt is used to understand if we can perform new memory requests
         * When branching, we flush both the FIFO and the outstanding requests. Therefore,
         * there is surely space for a new request.
         * Masking fifo_cnt in this case allows for making a new request when the FIFO
         * is not empty and we are jumping, and (fifo_cnt_i + cnt_q == DEPTH)
         */
    fifo_cnt_masked := Mux(io.fsInf.branch, 0.U, io.fifoInf.fifo_cnt)

    /** FSM to control OBI A (address) channel signals */
    io.transInf.transRequest.bits.addr := trans_addr_q

    switch(state) {
        is(idle) {
            /** Default state (pass on branch target address or transaction with incremented address) */
            when(io.fsInf.branch) {
                /** Jumps must have the highest priority */
                io.transInf.transRequest.bits.addr := aligned_branch_addr
            }.otherwise {
                io.transInf.transRequest.bits.addr := trans_addr_incr
            }
            when(io.fsInf.branch && !(io.transInf.transRequest.valid && io.transInf.transRequest.ready)) {
                /** Taken branch, but transaction not yet accepted by bus interface adapter */
                state := branchWait
            }
        }   // case idle
        is(branchWait) {
            /**
                 * Replay previous branch target address (trans_addr_q) or new branch address (this can
                 * occur if for example an interrupt is taken right after a taken jump which did not
                 * yet have its target address accepted by the bus interface adapter.
                 */
            io.transInf.transRequest.bits.addr := Mux(io.fsInf.branch, aligned_branch_addr, trans_addr_q)
            when(io.transInf.transRequest.valid && io.transInf.transRequest.ready) {
                /** Transaction with branch target address has been accepted. Start regular prefetch again */
                state := idle
            }
        }   // case branchWait
    }

    //////////////////////////////////////////////////////////////////////////////
    // FIFO management
    //////////////////////////////////////////////////////////////////////////////

    /**
         * Pass on response transfer directly to FIFO (which should be ready, otherwise
         * the corresponding transfer would not have been requested via trans_valid_o).
         * Upon a branch (branch_i) all incoming responses (resp_valid_i) are flushed
         * until the flush count is 0 again. (The flush count is initialized with the
         * number of outstanding transactions at the time of the branch).
         */
    fifo_valid := !io.fifoInf.fifo_empty
    io.fifoInf.fifo_push := io.transInf.transRespValid && (fifo_valid || !io.fetch_ready) && !(io.fsInf.branch || (flush_cnt > 0.U))
    io.fifoInf.fifo_pop := fifo_valid && io.fetch_ready

    //////////////////////////////////////////////////////////////////////////////
    // Counter (cnt_q, next_cnt) to count number of outstanding OBI transactions
    // (maximum = DEPTH)
    //
    // Counter overflow is prevented by limiting the number of outstanding transactions
    // to DEPTH. Counter underflow is prevented by the assumption that resp_valid_i = 1
    // will only occur in response to accepted transfer request (as per the OBI protocol).
    //////////////////////////////////////////////////////////////////////////////

    count_up := io.transInf.transRequest.valid && io.transInf.transRequest.ready
    count_down := io.transInf.transRespValid

    cnt := MuxLookup(Cat(count_up, count_down), cnt, 
        Seq(
            0.U(2.W) -> cnt,
            1.U(2.W) -> (cnt - 1.U(1.W)),
            2.U(2.W) -> (cnt + 1.U(1.W)),
            3.U(2.W) -> cnt
        )
    )

    /** Generate no hardware loop circuits */
    io.fifoInf.fifo_flush := io.fsInf.branch
    io.fifoInf.fifo_flush_but_first := false.B
    
    //////////////////////////////////////////////////////////////////////////////
    // Counter (flush_cnt_q, next_flush_cnt) to count reseponses to be flushed.
    //////////////////////////////////////////////////////////////////////////////

    /**
         * Number of outstanding transfers at time of branch equals the number of
         * responses that will need to be flushed (responses already in the FIFO will
         * be flushed there)
         */
    when(io.fsInf.branch) {
        flush_cnt := cnt
        when(io.transInf.transRespValid && (cnt > 0.U)) {
            flush_cnt := cnt - 1.U(1.W)
        }
    }.elsewhen(io.transInf.transRespValid && (flush_cnt > 0.U)) {
        flush_cnt := flush_cnt - 1.U(1.W)
    }

    //////////////////////////////////////////////////////////////////////////////
    // Registers
    //////////////////////////////////////////////////////////////////////////////
    when(io.fsInf.branch || (io.transInf.transRequest.valid && io.transInf.transRequest.ready)) {
        trans_addr_q := io.transInf.transRequest.bits.addr
    }
}

object PrefetchController extends App {
    (new ChiselStage).emitVerilog(new PrefetchController()(new IFParams()))
}
