package fpu
import spinal.core._
import spinal.lib._

case class FpuRoundMode() extends SpinalEnum(binarySequential) {
  val NEAREST      = newElement()
  val ZERO         = newElement()
  val POS_INFINITY = newElement()
  val NEG_INFINITY = newElement()
}

