/** 
  * See LICENSE for license details.
  * 
  * Author: Ruohui Chen
  * Date: 2021-06-23
  * Description: Defines for various constants used by the processor core.
  */

package megumi32.include

import chisel3._
import chisel3.util._

trait BasicParams {
    val PULP_OBI: Int       = 0     // Legacy PULP OBI behavior
    // val PULP_XPULP       = 0     // PULP ISA Extension (no use in our implementation)
    val XLEN: Int           = 32    // Width of an integer register in bits
    val FPU: Int            = 1
    val A_EXTENDSION        = 1
    val TRAP_LEN: Int       = 24
    val USE_PMP             = 0
    val APU_WOP_CPU         = 6     // In cva6 docs, this may have no use?
    val DEBUG_TRIGGER_EN    = 1

    /** OBI interface relative.*/
    val BE_WIDTH: Int   = 4     // Byte enable width
    val ATOP_WDITH: Int = 6     // Future proof addtion (no use now)
}

class IFParams(var depth: Option[Int] = None) extends BasicParams {
    var DEPTH: Int           = depth match {
        case Some(value) => value
        case None => 4
    }
    val FIFO_ADDR_DEPTH: Int = if (DEPTH > 1) log2Ceil(DEPTH) else 1

    /** Trap Mux Selector */
    val TRAP_MACHINE: UInt      = 0.U(2.W)
    val TRAP_USER: UInt         = 1.U(2.W)

    /** Exception PC mux selector defines */
    val EXC_PC_EXCEPTION        = 0.U(3.W)
    val EXC_PC_IRQ              = 1.U(3.W)
    val EXC_PC_DBD              = 2.U(3.W)
    val EXC_PC_DBE              = 3.U(3.W)

    /** PC mux selector defines */
    val PC_BOOT          = "b0000".U(4.W)
    val PC_JUMP          = "b0010".U(4.W)
    val PC_BRANCH        = "b0011".U(4.W)
    val PC_EXCEPTION     = "b0100".U(4.W)
    val PC_FENCEI        = "b0001".U(4.W)
    val PC_MRET          = "b0101".U(4.W)
    val PC_URET          = "b0110".U(4.W)
    val PC_DRET          = "b0111".U(4.W)
    val PC_HWLOOP        = "b1000".U(4.W)
}

class FIFOParams(depth: Option[Int] = None) extends IFParams(depth) {
    val FALL_THROUGH: UInt      = 0.U(1.W)  // FIFO is in fall-through mode
    val DATA_WIDTH: Int         = 32        // Default data width if the fifo is of type logic
    val ADDR_DEPTH: Int         = FIFO_ADDR_DEPTH
}

class ObiParams(depth: Option[Int] = None) extends IFParams(depth) {
    val TRANS_STABLE: Int = 0       // Are trans_addr_i, trans_we_i, trans_be_i, trans_wdata_i, trans_atop_i signals stable during a non-accepted transaction?
}

class IDParams extends BasicParams {}

class RegParams extends IDParams {
    val ADDR_WIDTH: Int         = if (FPU == 1) 6 else 5
    val READ_PORT_NUM: Int      = 3
    val WRITE_PORT_NUM: Int     = 2
}

object Opcode {
    val SYSTEM    = 0x73.U(7.W)
    val FENCE     = 0x0f.U(7.W)
    val OP        = 0x33.U(7.W)
    val OPIMM     = 0x13.U(7.W)
    val STORE     = 0x23.U(7.W)
    val LOAD      = 0x03.U(7.W)
    val BRANCH    = 0x63.U(7.W)
    val JALR      = 0x67.U(7.W)
    val JAL       = 0x6f.U(7.W)
    val AUIPC     = 0x17.U(7.W)
    val LUI       = 0x37.U(7.W)
    val OP_FP     = 0x53.U(7.W)
    val OP_FMADD  = 0x43.U(7.W)
    val OP_FNMADD = 0x4f.U(7.W)
    val OP_FMSUB  = 0x47.U(7.W)
    val OP_FNMSUB = 0x4b.U(7.W)
    val STORE_FP  = 0x27.U(7.W)
    val LOAD_FP   = 0x07.U(7.W)
    val AMO       = 0x2F.U(7.W)
}

object ALUOp {
    val width = 7
    
    val ADD   = "b0011000".U
    val SUB   = "b0011001".U
    val ADDU  = "b0011010".U
    val SUBU  = "b0011011".U
    val ADDR  = "b0011100".U
    val SUBR  = "b0011101".U
    val ADDUR = "b0011110".U
    val SUBUR = "b0011111".U

    val XOR   = "b0101111".U
    val OR    = "b0101110".U
    val AND   = "b0010101".U

    // Shifts
    val SRA   = "b0100100".U
    val SRL   = "b0100101".U
    val ROR   = "b0100110".U
    val SLL   = "b0100111".U

    // bit manipulation
    val BEXT  = "b0101000".U
    val BEXTU = "b0101001".U
    val BINS  = "b0101010".U
    val BCLR  = "b0101011".U
    val BSET  = "b0101100".U
    val BREV  = "b1001001".U

    // Bit counting
    val FF1   = "b0110110".U
    val FL1   = "b0110111".U
    val CNT   = "b0110100".U
    val CLB   = "b0110101".U

    // Sign-/zero-extensions
    val EXTS  = "b0111110".U
    val EXT   = "b0111111".U

    // Comparisons
    val LTS   = "b0000000".U
    val LTU   = "b0000001".U
    val LES   = "b0000100".U
    val LEU   = "b0000101".U
    val GTS   = "b0001000".U
    val GTU   = "b0001001".U
    val GES   = "b0001010".U
    val GEU   = "b0001011".U
    val EQ    = "b0001100".U
    val NE    = "b0001101".U

    // Set Lower Than operations
    val SLTS  = "b0000010".U
    val SLTU  = "b0000011".U
    val SLETS = "b0000110".U
    val SLETU = "b0000111".U

    // Absolute value
    val ABS   = "b0010100".U
    val CLIP  = "b0010110".U
    val CLIPU = "b0010111".U

    // Insert/extract
    val INS   = "b0101101".U

    // min/max
    val MIN   = "b0010000".U
    val MINU  = "b0010001".U
    val MAX   = "b0010010".U
    val MAXU  = "b0010011".U

    // div/rem
    val DIVU  = "b0110000".U // bit 0 is used for signed mode, bit 1 is used for remdiv
    val DIV   = "b0110001".U // bit 0 is used for signed mode, bit 1 is used for remdiv
    val REMU  = "b0110010".U // bit 0 is used for signed mode, bit 1 is used for remdiv
    val REM   = "b0110011".U // bit 0 is used for signed mode, bit 1 is used for remdiv

    val SHUF  = "b0111010".U
    val SHUF2 = "b0111011".U
    val PCKLO = "b0111000".U
    val PCKHI = "b0111001".U
}

object MULop {
    val width = 3

    val MAC32 = "b000".U
    val MSU32 = "b001".U
    val I     = "b010".U
    val IR    = "b011".U
    val DOT8  = "b100".U
    val DOT16 = "b101".U
    val H     = "b110".U
}

object CSRNum {
    val width = 12

    ///////////////////////////////////////////////////////
    // User CSRs
    ///////////////////////////////////////////////////////

    // User trap setup
    val CSR_USTATUS        = "h000".U        // Not included (PULP_SECURE = 0)

    // Floating Point
    val CSR_FFLAGS         = "h001".U        // Included if FPU = 1
    val CSR_FRM            = "h002".U        // Included if FPU = 1
    val CSR_FCSR           = "h003".U        // Included if FPU = 1

    // User trap setup
    val CSR_UTVEC          = "h005".U        // Not included (PULP_SECURE = 0)

    // User trap handling
    val CSR_UEPC           = "h041".U        // Not included (PULP_SECURE = 0)
    val CSR_UCAUSE         = "h042".U        // Not included (PULP_SECURE = 0)

    ///////////////////////////////////////////////////////
    // User Custom CSRs
    ///////////////////////////////////////////////////////

    // Hardware Loop
    val CSR_LPSTART0       = "h800".U        // Custom CSR. Included if PULP_HWLP = 1
    val CSR_LPEND0         = "h801".U        // Custom CSR. Included if PULP_HWLP = 1
    val CSR_LPCOUNT0       = "h802".U        // Custom CSR. Included if PULP_HWLP = 1
    val CSR_LPSTART1       = "h804".U        // Custom CSR. Included if PULP_HWLP = 1
    val CSR_LPEND1         = "h805".U        // Custom CSR. Included if PULP_HWLP = 1
    val CSR_LPCOUNT1       = "h806".U        // Custom CSR. Included if PULP_HWLP = 1

    // User Hart ID
    val CSR_UHARTID        = "hCC0".U        // Custom CSR. User Hart ID

    // Privilege
    val CSR_PRIVLV         = "hCC1".U        // Custom CSR. Privilege Level

    ///////////////////////////////////////////////////////
    // Machine CSRs
    ///////////////////////////////////////////////////////

    // Machine trap setup
    val CSR_MSTATUS        = "h300".U
    val CSR_MISA           = "h301".U
    val CSR_MIE            = "h304".U
    val CSR_MTVEC          = "h305".U

    // Performance counters
    val CSR_MCOUNTEREN     = "h306".U
    val CSR_MCOUNTINHIBIT  = "h320".U
    val CSR_MHPMEVENT3     = "h323".U
    val CSR_MHPMEVENT4     = "h324".U
    val CSR_MHPMEVENT5     = "h325".U
    val CSR_MHPMEVENT6     = "h326".U
    val CSR_MHPMEVENT7     = "h327".U
    val CSR_MHPMEVENT8     = "h328".U
    val CSR_MHPMEVENT9     = "h329".U
    val CSR_MHPMEVENT10    = "h32A".U
    val CSR_MHPMEVENT11    = "h32B".U
    val CSR_MHPMEVENT12    = "h32C".U
    val CSR_MHPMEVENT13    = "h32D".U
    val CSR_MHPMEVENT14    = "h32E".U
    val CSR_MHPMEVENT15    = "h32F".U
    val CSR_MHPMEVENT16    = "h330".U
    val CSR_MHPMEVENT17    = "h331".U
    val CSR_MHPMEVENT18    = "h332".U
    val CSR_MHPMEVENT19    = "h333".U
    val CSR_MHPMEVENT20    = "h334".U
    val CSR_MHPMEVENT21    = "h335".U
    val CSR_MHPMEVENT22    = "h336".U
    val CSR_MHPMEVENT23    = "h337".U
    val CSR_MHPMEVENT24    = "h338".U
    val CSR_MHPMEVENT25    = "h339".U
    val CSR_MHPMEVENT26    = "h33A".U
    val CSR_MHPMEVENT27    = "h33B".U
    val CSR_MHPMEVENT28    = "h33C".U
    val CSR_MHPMEVENT29    = "h33D".U
    val CSR_MHPMEVENT30    = "h33E".U
    val CSR_MHPMEVENT31    = "h33F".U

    // Machine trap handling
    val CSR_MSCRATCH       = "h340".U
    val CSR_MEPC           = "h341".U
    val CSR_MCAUSE         = "h342".U
    val CSR_MTVAL          = "h343".U
    val CSR_MIP            = "h344".U

    // Physical memory protection (PMP)
    val CSR_PMPCFG0        = "h3A0".U        // Not included (USE_PMP = 0)
    val CSR_PMPCFG1        = "h3A1".U        // Not included (USE_PMP = 0)
    val CSR_PMPCFG2        = "h3A2".U        // Not included (USE_PMP = 0)
    val CSR_PMPCFG3        = "h3A3".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR0       = "h3B0".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR1       = "h3B1".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR2       = "h3B2".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR3       = "h3B3".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR4       = "h3B4".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR5       = "h3B5".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR6       = "h3B6".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR7       = "h3B7".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR8       = "h3B8".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR9       = "h3B9".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR10      = "h3BA".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR11      = "h3BB".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR12      = "h3BC".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR13      = "h3BD".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR14      = "h3BE".U        // Not included (USE_PMP = 0)
    val CSR_PMPADDR15      = "h3BF".U        // Not included (USE_PMP = 0)

    // Trigger
    val CSR_TSELECT        = "h7A0".U
    val CSR_TDATA1         = "h7A1".U
    val CSR_TDATA2         = "h7A2".U
    val CSR_TDATA3         = "h7A3".U
    val CSR_TINFO          = "h7A4".U
    val CSR_MCONTEXT       = "h7A8".U
    val CSR_SCONTEXT       = "h7AA".U

    // Debug/trace
    val CSR_DCSR           = "h7B0".U
    val CSR_DPC            = "h7B1".U

    // Debug
    val CSR_DSCRATCH0      = "h7B2".U
    val CSR_DSCRATCH1      = "h7B3".U

    // Hardware Performance Monitor
    val CSR_MCYCLE         = "hB00".U
    val CSR_MINSTRET       = "hB02".U
    val CSR_MHPMCOUNTER3   = "hB03".U
    val CSR_MHPMCOUNTER4   = "hB04".U
    val CSR_MHPMCOUNTER5   = "hB05".U
    val CSR_MHPMCOUNTER6   = "hB06".U
    val CSR_MHPMCOUNTER7   = "hB07".U
    val CSR_MHPMCOUNTER8   = "hB08".U
    val CSR_MHPMCOUNTER9   = "hB09".U
    val CSR_MHPMCOUNTER10  = "hB0A".U
    val CSR_MHPMCOUNTER11  = "hB0B".U
    val CSR_MHPMCOUNTER12  = "hB0C".U
    val CSR_MHPMCOUNTER13  = "hB0D".U
    val CSR_MHPMCOUNTER14  = "hB0E".U
    val CSR_MHPMCOUNTER15  = "hB0F".U
    val CSR_MHPMCOUNTER16  = "hB10".U
    val CSR_MHPMCOUNTER17  = "hB11".U
    val CSR_MHPMCOUNTER18  = "hB12".U
    val CSR_MHPMCOUNTER19  = "hB13".U
    val CSR_MHPMCOUNTER20  = "hB14".U
    val CSR_MHPMCOUNTER21  = "hB15".U
    val CSR_MHPMCOUNTER22  = "hB16".U
    val CSR_MHPMCOUNTER23  = "hB17".U
    val CSR_MHPMCOUNTER24  = "hB18".U
    val CSR_MHPMCOUNTER25  = "hB19".U
    val CSR_MHPMCOUNTER26  = "hB1A".U
    val CSR_MHPMCOUNTER27  = "hB1B".U
    val CSR_MHPMCOUNTER28  = "hB1C".U
    val CSR_MHPMCOUNTER29  = "hB1D".U
    val CSR_MHPMCOUNTER30  = "hB1E".U
    val CSR_MHPMCOUNTER31  = "hB1F".U

    val CSR_MCYCLEH        = "hB80".U
    val CSR_MINSTRETH      = "hB82".U
    val CSR_MHPMCOUNTER3H  = "hB83".U
    val CSR_MHPMCOUNTER4H  = "hB84".U
    val CSR_MHPMCOUNTER5H  = "hB85".U
    val CSR_MHPMCOUNTER6H  = "hB86".U
    val CSR_MHPMCOUNTER7H  = "hB87".U
    val CSR_MHPMCOUNTER8H  = "hB88".U
    val CSR_MHPMCOUNTER9H  = "hB89".U
    val CSR_MHPMCOUNTER10H = "hB8A".U
    val CSR_MHPMCOUNTER11H = "hB8B".U
    val CSR_MHPMCOUNTER12H = "hB8C".U
    val CSR_MHPMCOUNTER13H = "hB8D".U
    val CSR_MHPMCOUNTER14H = "hB8E".U
    val CSR_MHPMCOUNTER15H = "hB8F".U
    val CSR_MHPMCOUNTER16H = "hB90".U
    val CSR_MHPMCOUNTER17H = "hB91".U
    val CSR_MHPMCOUNTER18H = "hB92".U
    val CSR_MHPMCOUNTER19H = "hB93".U
    val CSR_MHPMCOUNTER20H = "hB94".U
    val CSR_MHPMCOUNTER21H = "hB95".U
    val CSR_MHPMCOUNTER22H = "hB96".U
    val CSR_MHPMCOUNTER23H = "hB97".U
    val CSR_MHPMCOUNTER24H = "hB98".U
    val CSR_MHPMCOUNTER25H = "hB99".U
    val CSR_MHPMCOUNTER26H = "hB9A".U
    val CSR_MHPMCOUNTER27H = "hB9B".U
    val CSR_MHPMCOUNTER28H = "hB9C".U
    val CSR_MHPMCOUNTER29H = "hB9D".U
    val CSR_MHPMCOUNTER30H = "hB9E".U
    val CSR_MHPMCOUNTER31H = "hB9F".U

    val CSR_CYCLE          = "hC00".U
    val CSR_INSTRET        = "hC02".U
    val CSR_HPMCOUNTER3    = "hC03".U
    val CSR_HPMCOUNTER4    = "hC04".U
    val CSR_HPMCOUNTER5    = "hC05".U
    val CSR_HPMCOUNTER6    = "hC06".U
    val CSR_HPMCOUNTER7    = "hC07".U
    val CSR_HPMCOUNTER8    = "hC08".U
    val CSR_HPMCOUNTER9    = "hC09".U
    val CSR_HPMCOUNTER10   = "hC0A".U
    val CSR_HPMCOUNTER11   = "hC0B".U
    val CSR_HPMCOUNTER12   = "hC0C".U
    val CSR_HPMCOUNTER13   = "hC0D".U
    val CSR_HPMCOUNTER14   = "hC0E".U
    val CSR_HPMCOUNTER15   = "hC0F".U
    val CSR_HPMCOUNTER16   = "hC10".U
    val CSR_HPMCOUNTER17   = "hC11".U
    val CSR_HPMCOUNTER18   = "hC12".U
    val CSR_HPMCOUNTER19   = "hC13".U
    val CSR_HPMCOUNTER20   = "hC14".U
    val CSR_HPMCOUNTER21   = "hC15".U
    val CSR_HPMCOUNTER22   = "hC16".U
    val CSR_HPMCOUNTER23   = "hC17".U
    val CSR_HPMCOUNTER24   = "hC18".U
    val CSR_HPMCOUNTER25   = "hC19".U
    val CSR_HPMCOUNTER26   = "hC1A".U
    val CSR_HPMCOUNTER27   = "hC1B".U
    val CSR_HPMCOUNTER28   = "hC1C".U
    val CSR_HPMCOUNTER29   = "hC1D".U
    val CSR_HPMCOUNTER30   = "hC1E".U
    val CSR_HPMCOUNTER31   = "hC1F".U

    val CSR_CYCLEH         = "hC80".U
    val CSR_INSTRETH       = "hC82".U
    val CSR_HPMCOUNTER3H   = "hC83".U
    val CSR_HPMCOUNTER4H   = "hC84".U
    val CSR_HPMCOUNTER5H   = "hC85".U
    val CSR_HPMCOUNTER6H   = "hC86".U
    val CSR_HPMCOUNTER7H   = "hC87".U
    val CSR_HPMCOUNTER8H   = "hC88".U
    val CSR_HPMCOUNTER9H   = "hC89".U
    val CSR_HPMCOUNTER10H  = "hC8A".U
    val CSR_HPMCOUNTER11H  = "hC8B".U
    val CSR_HPMCOUNTER12H  = "hC8C".U
    val CSR_HPMCOUNTER13H  = "hC8D".U
    val CSR_HPMCOUNTER14H  = "hC8E".U
    val CSR_HPMCOUNTER15H  = "hC8F".U
    val CSR_HPMCOUNTER16H  = "hC90".U
    val CSR_HPMCOUNTER17H  = "hC91".U
    val CSR_HPMCOUNTER18H  = "hC92".U
    val CSR_HPMCOUNTER19H  = "hC93".U
    val CSR_HPMCOUNTER20H  = "hC94".U
    val CSR_HPMCOUNTER21H  = "hC95".U
    val CSR_HPMCOUNTER22H  = "hC96".U
    val CSR_HPMCOUNTER23H  = "hC97".U
    val CSR_HPMCOUNTER24H  = "hC98".U
    val CSR_HPMCOUNTER25H  = "hC99".U
    val CSR_HPMCOUNTER26H  = "hC9A".U
    val CSR_HPMCOUNTER27H  = "hC9B".U
    val CSR_HPMCOUNTER28H  = "hC9C".U
    val CSR_HPMCOUNTER29H  = "hC9D".U
    val CSR_HPMCOUNTER30H  = "hC9E".U
    val CSR_HPMCOUNTER31H  = "hC9F".U

    // Machine information
    val CSR_MVENDORID      = "hF11".U
    val CSR_MARCHID        = "hF12".U
    val CSR_MIMPID         = "hF13".U
    val CSR_MHARTID        = "hF14".U
}



// FP OP Group
object FPOpGroup{
    val width = 2

    val ADDMUL         = 0.U(2.W)
    val DIVSQRT        = 1.U(2.W)
    val NONCOMP        = 2.U(2.W)
    val CONV           = 3.U(2.W)
}

object APU {
    // APU interface 
    val APU_NARGS_CPU: Int          = 3
    val APU_WOP_CPU :Int            = 6
    val APU_NDSFLAGS_CPU: Int       = 15
    val APU_NUSFLAGS_CPU :Int       = 5

    // addsub
    val PIPE_REG_ADDSUB: Int        = 1
    // mult
    val PIPE_REG_MULT :Int          = 1
    // casts
    val PIPE_REG_CAST: Int          = 1
    // mac
    val PIPE_REG_MAC :Int           = 2
}

object FPExtenConfi{
    //Floating-point extensions configuration
    val C_RVF                       = true.B //Is F extension enable
    val C_RVD                       = false.B //Is D extension enable
    //Transprecision floating-point extensions configuration
    val C_XF16                      = false.B  //Is half-precision float extension (XF16) enable
    val C_XF16ALT                   = false.B  //Is alternative half-precision float extension (Xf16alt) enabled
    val C_XF8                       = false.B  //Is quarter-precision float extension (Xf8) enabled
    val C_XFVEC                     = false.B //Is vectorial float extension (Xfvec) enabled
}

//?????????????????
object FPOpLatency{
    val C_LAT_FP64                      = false.B 
    val C_LAT_FP32                      = false.B 
    val C_LAT_FP16                      = false.B 
    val C_LAT_FP16ALT                   = false.B 
    val C_LAT_FP8                       = false.B 
    val C_LAT_DIVSQRT                   = true.B  // divsqrt post-processing pipe
    val C_LAT_CONV                      = false.B 
    val C_LAT_NONCOMP                   = false.B 
}

object FPU {
    // ---------
    // FP TYPES
    // ---------
    // | Enumerator | Format           | Width  | EXP_BITS | MAN_BITS
    // |:----------:|------------------|-------:|:--------:|:--------:
    // | FP32       | IEEE binary32    | 32 bit | 8        | 23
    // | FP64       | IEEE binary64    | 64 bit | 11       | 52
    // | FP16       | IEEE binary16    | 16 bit | 5        | 10
    // | FP8        | binary8          |  8 bit | 5        | 2
    // | FP16ALT    | binary16alt      | 16 bit | 8        | 7
    // *NOTE:* Add new formats only at the end of the enumeration for backwards compatibilty!

    val NUM_FP_FORMATS: Int         = 5     // Change me to add formats
    val FP_FORMAT_BITS: Int         = log2Ceil(NUM_FP_FORMATS)

    /** FP formats */
    val FP32        = 0.U(FP_FORMAT_BITS.W)
    val FP64        = 1.U(FP_FORMAT_BITS.W)
    val FP16        = 2.U(FP_FORMAT_BITS.W)
    val FP8         = 3.U(FP_FORMAT_BITS.W)
    val FP16ALT     = 4.U(FP_FORMAT_BITS.W)


    // ---------
    // INT TYPES
    // ---------
    // | Enumerator | Width  |
    // |:----------:|-------:|
    // | INT8       |  8 bit |
    // | INT16      | 16 bit |
    // | INT32      | 32 bit |
    // | INT64      | 64 bit |
    // *NOTE:* Add new formats only at the end of the enumeration for backwards compatibilty!
    val NUM_INT_FORMATS: Int        = 4     // Change me to add formats
    val INT_FORMAT_BITS: Int        = log2Ceil(NUM_INT_FORMATS)

    /** Int formats */
    val INT8        = 0.U(INT_FORMAT_BITS.W)
    val INT16       = 1.U(INT_FORMAT_BITS.W)
    val INT32       = 2.U(INT_FORMAT_BITS.W)
    val INT64       = 3.U(INT_FORMAT_BITS.W)

    val C_RM: Int                   = 3

    /** FP operations */
    val OP_BITS     = 4
    
    val FMADD       = 0.U(OP_BITS.W)
    val FNMSUB      = 1.U(OP_BITS.W)
    val ADD         = 2.U(OP_BITS.W)
    val MUL         = 3.U(OP_BITS.W)
    val DIV         = 4.U(OP_BITS.W)
    val SQRT        = 5.U(OP_BITS.W)
    val SGNJ        = 6.U(OP_BITS.W)
    val MINMAX      = 7.U(OP_BITS.W)
    val CMP         = 8.U(OP_BITS.W)
    val CLASSIFY    = 9.U(OP_BITS.W)
    val F2F         = 10.U(OP_BITS.W)
    val F2I         = 11.U(OP_BITS.W)
    val I2F         = 12.U(OP_BITS.W)
    val CPKAB       = 13.U(OP_BITS.W)
    val CPKCD       = 14.U(OP_BITS.W)
}

object CSROp {
    val width = 2

    val CSR_OP_READ     = "b00".U
    val CSR_OP_WRITE    = "b01".U
    val CSR_OP_SET      = "b10".U
    val CSR_OP_CLEAR    = "b11".U
}

object PrivLvl {
    val width = 2

    val PRIV_LVL_M = "b11".U
    val PRIV_LVL_H = "b10".U
    val PRIV_LVL_S = "b01".U
    val PRIV_LVL_U = "b00".U
}

object CtrlOpConstans {
    /** Bool */
    val N = 0.U(1.W)
    val Y = 1.U(1.W)

    /** Operand A selection */
    val OP_A_REGA_OR_FWD = "b000".U(3.W)
    val OP_A_CURRPC      = "b001".U(3.W)
    val OP_A_IMM         = "b010".U(3.W)
    val OP_A_REGB_OR_FWD = "b011".U(3.W)
    val OP_A_REGC_OR_FWD = "b100".U(3.W)

    /** Operand B selection */
    val OP_B_REGB_OR_FWD = "b000".U(3.W)
    val OP_B_REGC_OR_FWD = "b001".U(3.W)
    val OP_B_IMM         = "b010".U(3.W)
    val OP_B_REGA_OR_FWD = "b011".U(3.W)
    val OP_B_BMASK       = "b100".U(3.W)

    /** Operand C selection */
    val OP_C_REGC_OR_FWD = "b00".U(2.W)
    val OP_C_REGB_OR_FWD = "b01".U(2.W)
    val OP_C_JT          = "b10".U(2.W)

    /** Immediate A selection */
    val IMMA_Z           = 0.U(1.W)
    val IMMA_ZERO        = 1.U(1.W)

    /** Immediate B selection */
    val IMMB_I      = "b0000".U(4.W)
    val IMMB_S      = "b0001".U(4.W)
    val IMMB_U      = "b0010".U(4.W)
    val IMMB_PCINCR = "b0011".U(4.W)
    val IMMB_S2     = "b0100".U(4.W)
    val IMMB_S3     = "b0101".U(4.W)
    val IMMB_VS     = "b0110".U(4.W)
    val IMMB_VU     = "b0111".U(4.W)
    val IMMB_SHUF   = "b1000".U(4.W)
    val IMMB_CLIP   = "b1001".U(4.W)
    val IMMB_BI     = "b1011".U(4.W)

    /** regCMux selection */
    val REGC_S1     = "b10".U(2.W)
    val REGC_S4     = "b00".U(2.W)
    val REGC_RD     = "b01".U(2.W)
    val REGC_ZERO   = "b11".U(2.W)

    /** Multiplication immediates */
    val MIMM_ZERO   = 0.U(1.W)
    val MIMM_S3     = 1.U(1.W)

    /** Branch types */
    val BRANCH_NONE = "b00".U(2.W)
    val BRANCH_JAL  = "b01".U(2.W)
    val BRANCH_JALR = "b10".U(2.W)
    val BRANCH_COND = "b11".U(2.W) // conditional branches

    /** Jump target mux */
    val JT_JAL      = "b01".U(2.W)
    val JT_JALR     = "b10".U(2.W)
    val JT_COND     = "b11".U(2.W)
}