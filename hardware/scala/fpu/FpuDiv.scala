package fpu

import spinal.core._
import spinal.lib._

class FpuDiv(config: FpuConfig) extends Component {
  import config._

  val io = new Bundle {
    val opa = in (Floating(exponentSize, mantissaSize)),
    val opb = in (Floating(exponentSize, mantissaSize)),
    val out = out (Floating(exponentSize, mantissaSize + 4)),
    val start = in Bool,
    val done = out Bool
  }

  val sign = Bool
  val mantissa_7 = Bits(mantissaSize + 4 bits)
  val exponent_out = Reg(Bits(exponentSize bits)) init(0)

  out.sign := io.opa.sign ^ io.opb.sign

  val a_is_norm = io.opa.orR
  val b_is_norm = io.opa.orR
  val a_is_zero = io.opa.isZero
  val exponent_a = B(0, 1 bit) ## io.opa.exponent_a
  val exponent_b = B(0, 1 bit) ## io.opa.exponent_b
  val dividend_denorm = divisor_b_shifted ## B(0, 1 bit)
  val dividend_1 = a_is_norm ? (B"01" ## dividend_a) | (B"0" ## dividend_denorm)
  val divisor_denorm = divisor_b_shifted ## B(0, 1 bit)
  val divisor_1 = b_is_norm ? (B"01" ## divisor_b) | (B"0" ## divisor_denorm)
  val count_nonzero = count_index === 0 ? B(0, 1 bit) | B(1, 1 bit)
  val count_index = count_out
  val quotient_msb = quotient_out(mantissaSize + 1)
  val mantissa_2 = quotient_out(mantissaSize downto 1)
  val mantissa_3 = quotient_out(mantissaSize - 1 downto 1)
  val mantissa_4 = quotient_msb ? mantissa_2 | mantissa_3
  val mantissa_5 = (expon_final_4 == 1) ? mantissa_2 | mantissa_4
  val mantissa_6 = expon_final_4_et0 ? mantissa_1 | mantissa_5
  val remainder_a = quotient_out(mantissaSize + 1 downto 0) ## remainder_msb ## remainder_out(mantissaSize downto 0)
  val remainder_1 = remainder_b(2*mantissaSize + 3 downto mantissaSize + 1) ## remainder_b(mantissaSize downto 0).orR
  val remainder_2 = quotient_out(0) & remainder_msb & remainder_out(mantissaSize downto 0) ## B(0, 1 bit)
  val remainder_3 = remainder_msb & remainder_out(mantissaSize downto 0) & B(0, 1 bits)
  val remainder_4 = quotient_msb ? remainder_2 | remainder_3
  val remainder_5 = expon_final_4 === 1 ? remainder_2 | remainder_4
  val remainder_6 = expon_final_4_et0 ? remainder_1 | remainder_5
  val m_norm = expon_final_5.orR
  val rem_lsb = remainder_6(mantissaSize + 2 downto 0).orR
  val mantissa_7 = Bit(0, 1 bit) ## m_norm ## mantissa_6 ## remainder_6(mantissaSize + 3) ## rem_lsb

  val count_out = Reg(Bits(log2up(mantissaSize bits))) init(0)

  when (io.start) {
    count_out  := 53
  } elsewhen (count_nonzero) {
    count_out  := count_out - 1 
  }

  val mantissa_1 = quotient_out(mantissaSize + 1 downto 2) >> expon_uf_term_4
  
  val expon_shift_a = Reg(Bits(exponentSize bits)) init(0)
  val expon_shift_b = Reg(Bits(exponentSize bits)) init(0)
  val expon_final_1 = Reg(Bits(exponentSize bits)) init(0)
  val expon_final_2 = Reg(Bits(exponentSize bits)) init(0)
  val expon_final_3 = Reg(Bits(exponentSize bits)) init(0)
  val expon_final_4 = Reg(Bits(exponentSize bits)) init(0)
  val expon_final_5 = Reg(Bits(exponentSize bits)) init(0)
  val expon_term = Reg(Bits(exponentSize bits)) init(0)
  val dividend_shift = Reg(Bits(log2up(mantissaSize bits))) init(0)
  val dividend_shift_2 = Regnext(dividend_shift)
  val divisor_shift = Reg(Bits(log2up(mantissaSize bits))) init(0)
  val divisor_shift_2 = Regnext(dividend_shift)
  val divisor_a_shifted = Reg(Bits(log2up(mantissaSize bits))) init(0)
  val divisor_b_shifted = Reg(Bits(log2up(mantissaSize bits))) init(0)
  val expon_final_4_term = Reg(Bool) init(False)
  val expon_final_4_et0 = Reg(Bool) init(False)
  val expon_uf_1 = Reg(Bool) init(False)
  val expon_uf_2 = Reg(Bool) init(False)
  val dividend_a = io.opa.mantissa
  val divisor_b = io.opb.mantissa
  expon_uf_1  := (exponent_b > expon_term)
  expon_uf_2  := (expon_shift_a > expon_final_2)
  exponent_out := a_is_zero ? 0 | expon_final_5
  expon_final_5 := (quotient_msb) ? expon_final_4 | (expon_final_4  - expon_final_4_term)
  expon_final_4_term := (expon_final_4_et0) ? False | True
  expon_final_4_et0 := (expon_final_4 == 0) ? True | False
  expon_final_4 := expon_final_3 + expon_shift_b
  expon_final_3 := expon_uf_2 ? 0 | (expon_final_2 - expon_shift_a)
  expon_final_2 := expon_uf_1 ? 0 | expon_final_1
  expon_term  := exponent_a + 1023
  expon_final_1 := expon_term - exponent_b
  expon_shift_a  := a_is_norm ? 0 | dividend_shift_2
  dividend_shift := count_1_zeros(dividend_a)
  divisor_shift := count_1_zeros(dividend_a)

  dividend_a_shifted := dividend_a << dividend_shift_2;
  divisor_b_shifted := divisor_b << divisor_shift_2;


}




