package fpu

import spinal.core._
import spinal.lib._

case class FpuOpcode() extends SpinalEnum(binarySequential) {
  val ADD = newElement()
  val SUB = newElement()
  val MUL = newElement()
  val DIV = newElement()
}


