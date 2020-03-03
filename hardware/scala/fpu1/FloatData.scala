package fpu
import spinal.core._
import spinal.lib._

case class FloatData(exponentSize: Int,
                     mantissaSize: Int) extends Bundle {

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

  def isUnderflow = (exponent <= -mantissaSize)

  /** Return true if number is +/- 0 */
  def isZero = isMantissaZero && isExponentZero

  def clear = {
    sign  := False
    exponent  := 0
    mantissa  := 0
  }

  def init(): this.type = {
    val initValue = cloneOf(this)
    initValue.clear
    this init (initValue)
    this
  }

  def NormalizedFlow = {
    when (isUnderflow) {
      clear
    } elsewhen (isExponentNegative) {
      shiftMentissaRight
    } elsewhen (isExponentPositive) {
      when (! isNormalized) {
        shiftMentissaLeft
      }
    }

    val ret = Flow(this)
    ret.payload := this
    ret.valid  := isNormalized || isExponentZero
    ret
  }
}






