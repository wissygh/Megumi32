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