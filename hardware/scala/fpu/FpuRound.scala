package fpu
import spinal.core._
import spinal.lib._


class FpuRound(config: FpuConfig) extends Component {
  import config._

  val io = new Bundle {
    val float_in = in (Floating(exponentSize + 1, mantissaSize + 4)),
    val round_out = out (Floating(exponentSize, mantissaSize)),
    val round_mode = in (FpuRoundMode()),
    val exponent_final = out Bits(exponentSize  bit),
  }

  val sum_final = Reg(Bits((mantissaSize + 4) bit)) init (0)
  val sum_round = Reg(Bits((mantissaSize + 4) bit)) init (0)
  val sum_round_2 = Reg(Bits((mantissaSize + 4) bit)) init (0)
  val exponent_round = Reg(Bits(exponentSize + 1) bit) init (0)
  val exponent_final_2 = Reg(Bits(exponentSize + 1) bit) init (0)

  val rounding_amount = B(4, (mantissaSize + 4) bit) // 56'b100

  val round_nearest = (io.round_mode == NEAREST)
  val round_to_zero = (io.round_mode == ZERO)
  val round_pos_inf = (io.round_mode == POS_INFINITY)
  val round_neg_inf = (io.round_mode == NEG_INFINITY)
  val round_nearest_trigger = (round_nearest && io.float_in.mantissa(1))
  val round_to_pos_inf_trigger = (!io.float_in.sign && io.float_in.mantissa(1 downto 0) == B"01")
  val round_to_neg_inf_trigger = (io.float_in.sign && io.float_in.mantissa(1 downto 0) == B"01")
  val round_nearest_trigger = (round_nearest && io.float_in.mantissa(1))
  val round_trigger = (round_nearest && round_nearest_trigger) ||
                      (round_pos_inf && round_to_pos_inf_trigger) ||
                      (round_neg_inf && round_to_neg_inf_trigger)

  val sum_round_overflow = sum_round((mantissaSize + 4) - 1)
  
  sum_round := rounding_amount + io.float_in.mantissa
  when (sum_round_overflow) {
    sum_round_2  := (sum_round >> 1)
    exponent_round  := io.float_in.exponent + B(1, (mantissaSize + 4) bit)
  } otherwise {
    sum_round_2  := sum_round 
    exponent_round  := io.float_in.exponent
  }

  when (round_trigger) {
    sum_final  := sum_round_2  
    exponent_final_2 := exponent_round 
  } otherwise {
    sum_final  := io.float_in.mantissa
    exponent_final_2 := io.float_in.exponent
  }

  io.exponent_final  := exponent_final_2
  io.round_out.sign = io.float_in.sign
  io.round_out.exponent = exponent_final_2(exponentSize - 1 downto 0)
  io.round_out.sign = sum_final(mantissaSize + 1 downto 2)
}



