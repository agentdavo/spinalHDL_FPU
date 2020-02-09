package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.experimental.math._

class FpuSub(config: FpuConfig) extends Component {
  import config._

  val io = new Bundle {
    val opa = in (Floating(exponentSize, mantissaSize)),
    val opb = in (Floating(exponentSize, mantissaSize)),
    val out = out (Floating(exponentSize, mantissaSize + 4)),
    val fpu_op = in (FpuOpcode()),
    val start = in Bool,
    val done = out Bool
  }

  val done = Reg(Bool) init(False)

  val op_small = Reg(Floating(exponentSize, mantissaSize))
  val op_large = Reg(Floating(exponentSize, mantissaSize))
  val expa_gt_expb = Reg(Bool) init (False)
  val expa_et_expb = Reg(Bool) init (False)
  val mana_gtet_manb = Reg(Bool) init (False)
  val a_gtet_b = Reg(Bool) init (False)
  val sub_out = Reg(Floating(exponentSize, mantissaSize))
  val fpu_op_add = io.fpu_op == FpuOpcode.ADD

  val fsm = new StateMachine{
    val START : State = new State with EntryPoint {
      whenIsActive {
        done  := False;
        when (io.start) {
          goto(COMPARE)
        }
      }
      onExit {
        expa_gt_expb  := io.opa.exponent > io.opb.exponent
        expa_et_expb  := io.opa.exponent === io.opb.exponent
        mana_gtet_manb  := io.opa.mantissa >= io.opb.mantissa
        a_gtet_b  := expa_gt_expb || (expa_et_expb && mana_gtet_manb)
      }
    }

    val small_is_denorm = Reg(Bool) init (False)
    val large_is_denorm = Reg(Bool) init (False)

    val COMPARE : State = new State {
      whenIsActive {
        when (a_gtet_b) {
          op_small  := io.opb
          op_large  := io.opa
          sub_out.sign := io.opa.sign
        } otherwise {
          op_small  := io.opa
          op_large  := io.opb
          sub_out.sign := (!io.opb.sign) ^ (fpu_op_add)
        }

        goto(SUB_1)
      }
      onExit {
      }
    }

    val large_norm_small_denorm = Reg(Bool) init (False)
    val small_is_nonzero = Reg(Bool) init (False)
    val exponent_diff = Reg(Bits(exponentSize bits)) init (B(0, exponentSize bits))
    val minuend = Reg(Bits(mantissaSize + 3 bits)) init (B(0, mantissaSize + 3 bits))
    val subtrahend = cloneOf(minuend)
    val subtra_shift = cloneOf(minuend)
    val subtra_shift_nonzero = subtra_shift.orR
    val subtra_fraction_enable = small_is_nonzero && !subtra_shift_nonzero
    val subtra_shift_3 = cloneOf(subtra_shift)

    val SUB_1 : State = new State {
      whenIsActive {
        small_is_denorm  := (op_small.exponent > B"0x0")
        large_is_denorm  := (op_large.exponent > B"0x0")
        goto(SUB_2)
      }
    }

    val SUB_2 : State = new State {
      whenIsActive {
        large_norm_small_denorm  := (small_is_denorm  && !large_is_denorm)
        small_is_nonzero  := (!small_is_denorm) || op_small.mantissa.orR
        minuend  := (!large_is_denorm) ## op_large.mantissa ## B(0, 2 bits)
        subtrahend  := (!small_is_denorm) ## op_small.mantissa ## B(0, 2 bits)
        goto(SUB_3)
      }
    }

    val SUB_3 : State = new State {
      whenIsActive {
        exponent_diff  := op_large.exponent - op_small.exponent - large_norm_small_denorm
        goto(SUB_4)
      }
    }

    val SUB_4 : State = new State {
      whenIsActive {
        subtra_shift  := subtrahend >> exponent_diff
        goto(SUB_5)
      }
    }

    val SUB_5 : State = new State {
      whenIsActive {
        subtra_shift_3  := subtra_fraction_enable ? B(1, mantissaSize + 3 bits) | subtra_shift
        goto(SUB_6)
      }
    }

    val diff = cloneOf(minuend)
    val SUB_6 : State = new State {
      whenIsActive {
        diff := minuend - subtra_shift_3
        goto(SUB_7)
      }
    }

    val diff_shift = Reg(Bits(log2up(mantissaSize + 3) bits))
    val SUB_7 : State = new State {
      onEntry {
      }
      whenIsActive {
        diff_shift  := count_1_zeros(diff) // Count number of zeros before leftmost 1 in mantissa bits
        goto(SUB_8)
      }
    }

    val diff_shift_2 = cloneOf(diff_shift)
    val SUB_8 : State = new State {
      whenIsActive {
        diff_shift_2  := diff_shift
        goto(SUB_9)
      }
    }

    val diffshift_gt_exponent = Reg(Bool) init (False)
    val diffshift_et_mantissaSize_plus_3 = Reg(Bool) init (False)
    val SUB_9 : State = new State {
      whenIsActive {
        diffshift_gt_exponent  := diff_shift_2 > op_large.exponent
        diffshift_et_mantissaSize_plus_3 := diff_shift_2 === (mantissaSize + 3)
        goto(SUB_10)
      }
    }

    val diff_1 = Reg(Bits(mantissaSize + 3 bits)) init (B(0, mantissaSize + 3 bits))
    val exponent = Reg(Bits(exponentSize bits))
    val SUB_10 : State = new State {
      whenIsActive {
        when (diffshift_gt_exponent) {
          diff_1 := diff << op_large.exponent
          exponent := B(0, exponentSize bits)
        } otherwise {
          diff_1 := diff << diff_shift_2
          exponent := op_large.exponent - diff_shift_2
        }

        when (diffshift_et_mantissaSize_plus_3) {
          sub_out.exponent := B(0, exponentSize bits)
        } otherwise {
          sub_out.exponent  := exponent
        }

        goto(DONE)
      }
    }

    val in_norm_out_denorm = op_large.exponent.orR && !exponent.orR
    val DONE : State = new State {
      whenIsActive {
        sub_out.mantissa = B(0, 1 bit) ## (in_norm_out_denorm ? (diff_1 >> 1) | diff_1)
        done := True;
        goto(START)
      }
    }
  }

  io.out := sub_out
  io.done := done;

}




