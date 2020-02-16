package fpu
import spinal.core._
import spinal.lib._

case class FloatData(exponentSize: Int,
                     mantissaSize: Int,
                     exponentBias: Int) extends Bundle {

  /** Mantissa field with implicit first bit */
  val mantissa = UInt(mantissaSize bits)

  /** Signed Exponent field */
  val exponent = SInt(exponentSize bits)

  /** Sign field (true when negative) */
  val sign = Bool

  def isExponentZero = exponent === 0
  def isMantissaZero = mantissa === 0

  def isExponentNegative = exponent < 0
  def isExponentPositive = exponent > 0

  def shiftMentissaRight = {
    mantissa  := mantissa  |>> 1
    exponent  := exponent  + 1
  }

  def shiftMentissaLeft = {
    mantissa  := mantissa  |<< 1
    exponent  := exponent  - 1
  }

  def isNormalized = mantissa(mantissaSize -1) === True

  def isUnderflow = (exponent < -exponentBias)

  /** Return true if number is +/- 0 */
  def isZero = isMantissaZero && isExponentZero

  def clear = {
    sign  := False
    exponent  := 0
    mantissa  := 0
  }

  def NormalizedFlow = {
    val ret = RegFlow(this)

    when (isUnderflow) {
      clear
    } elsewhen (isExponentNegative) {
      shiftMentissaRight
    } elsewhen (isExponentPositive) {
      when (! isNormalized) {
        shiftMentissaLeft
      }
    }

    when (isNormalized || isExponentZero) {
      ret.valid := True
      ret.payload := this
    }
    ret
  }
}






