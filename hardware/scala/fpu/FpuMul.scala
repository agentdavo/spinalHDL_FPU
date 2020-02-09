package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.experimental.math._
import scala.collection.mutable.ListBuffer
import spinal.lib.math._

class FpuMul(config: FpuConfig) extends Component {
  import config._

  val io = new Bundle {
    val opa = in (Floating(exponentSize, mantissaSize)),
    val opb = in (Floating(exponentSize, mantissaSize)),
    val out = out (Floating(exponentSize, mantissaSize + 4)),
    val start = in Bool,
    val done = out Bool
  }

  val out = Reg(Floating(exponentSize, mantissaSize + 4)) init(0)
  out.sign := io.opa.sign ^ io.opb.sign
  out.mantissa := B(0, 1 bit) ## product_6(105 downto 52) ## product_lsb
  out.exponent := exponent_5

  io.out := out

  val fsm = new StateMachine{
    val START : State = new State with EntryPoint{
      whenIsActive {
        when (start) {
          goto(MUL_1)
        }
      }
    }

    val exponent_a = Reg(Bits((exponentSize + 1) bits)) init (B"0")
    val exponent_b = Reg(Bits((exponentSize + 1) bits)) init (B"0")
    val exponent_term = Reg(Bits((exponentSize + 1) bits)) init (B"0")
    val a_is_norm = Reg(Bool) init(False)
    val b_is_norm = Reg(Bool) init(False)
    val in_zero = Reg(Bool) init(False)

    val MUL_1 : State = new State{
      onEntry {
        exponent_a  := B(0, 1 bit) ## io.opa.exponent
        exponent_b  := B(0, 1 bit) ## io.opb.exponent
        a_is_norm  := io.opa.exponent.orR
        b_is_norm  := io.opb.exponent.orR
        in_zero := io.opa.isZero || io.opb.isZero
      }
      whenIsActive {
        exponent_term  := exponent_a + exponent_b + (!a_is_norm) + (!b_is_norm)
        goto(MUL_2)
      }
    }

    val exponent_gt_expoffset = Reg(Bool) init(False)
    val exponent_under = Reg(Bits((exponentSize + 1) bits)) init (B(0))
    val exponent_1 = Reg(Bits((exponentSize + 1) bits)) init (B(0))
    val MUL_2 : State = new State{
      whenIsActive {
        exponent_gt_expoffset  := exponent_term >= (io.opa.getExponentBias - 1)
        exponent_under := (io.opa.getExponentBias - 1) - exponent_term
        exponent_1  := exponent_term - (io.opa.getExponentBias - 1)
        when (in_zero) {
          exponent_5 := B(0)
        } otherwise {
          exponent_5 := exponent_4
        }
        goto(MUL_3)
      }
    }

    val mul_a = Reg(Bits((mantissaSize + 1) bits)) init (B(0))
    val mul_b = Reg(Bits((mantissaSize + 1) bits)) init (B(0))
    val exponent_2 = Reg(Bits((exponentSize + 1) bits)) init (B(0))
    val MUL_3 : State = new State{
      whenIsActive {
        when (exponent_gt_expoffset) {
          exponent_2 := exponent_1
        } otherwise {
          exponent_2 := B(0)
        }
        mul_a  := a_is_norm ## io.opa.mantissa
        mul_b  := b_is_norm ## io.opb.mantissa
        goto(MUL_4)
      }
    }

    val product = Reg(Bits(2*(mantissaSize + 1) bits)) init (B(0))
    val mul_ab_count = Reg(UInt(16 bits)) init(U(0))
    val MUL_4 : State = new State{
      onEntry{
        mul_ab_count  := U(0)
      }
      whenIsActive {
        product = SIntMath.mul(mul_a, mul_b, 17, 0, 1, (stage, level) => RegNext(stage))
        val mul_ab_latency = LatencyAnalysis(mul_a, product)
        mul_ab_count  := mul_ab_count + 1
        when (mul_ab_count == mul_ab_latency) {
          goto(PRODUCT.MUL_5)
        }
      }
    }

    val product_shift = Bits(log2up(mantissaSize) bits)
    val product_1 = Reg(Bits(2*(mantissaSize + 1) bits)) init (B(0))
    val product_2 = Reg(Bits(2*(mantissaSize + 1) bits)) init (B(0))
    val MUL_5 : State = new State{
      whenIsActive {
        product_shift  := count_zeros_mul(product)
        product_1 = product >> exponent_under
        when (exponent_gt_expoffset) {
          product_2 := product
        } otherwise {
          product_2 := product_1
        }
        goto(MUL_6)
      }
    }

    val MUL_7 : State = new State{
      whenIsActive {
        product_shift_2  := product_shift
        goto(MUL_8)
      }
    }


    val product_3 = Reg(Bits(2*(mantissaSize + 1) bits)) init (B(0))
    val product_4 = Reg(Bits(2*(mantissaSize + 1) bits)) init (B(0))
    val MUL_9 : State = new State{
      whenIsActive {
        product_3  := product_2 << product_shift_2
        product_4  := product_2 << exponent_2
        goto(MUL_10)
      }
    }

    val MUL_10 : State = new State{
      whenIsActive {
        when (exponent_2 > product_shift_2) {
          exponent_gt_prodshift  := True
        } otherwise {
          exponent_gt_prodshift  := False
        }
        goto(MUL_11)
      }
    }

    val product_5 = Reg(Bits(2*(mantissaSize + 1) bits)) init (B(0))
    val MUL_11 : State = new State{
      whenIsActive {
        when (exponent_gt_prodshift == True) {
          product_5  := product_3
        } otherwise {
          product_5  := product_4
        }
        goto(MUL_12)
      }
    }

    val product_6 = Reg(Bits(2*(mantissaSize + 1) bits)) init (B(0))
    val MUL_12 : State = new State{
      whenIsActive {
        when (exponent_et_zero == True) {
          product_6  := product_5 >> 1
        } otherwise {
          product_6  := product_5
        }
        goto(MUL_13)
      }
    }

  }

}









