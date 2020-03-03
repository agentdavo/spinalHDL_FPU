package fpu
import spinal.core._
import spinal.lib._

class FpuNormalizer(exponentSize: Int,
                    mantissaSize: Int) extends Component {
  val inFloat = slave Flow (FloatData(exponentSize, mantissaSize))
  val outFloat = master Flow (FloatData(exponentSize, mantissaSize))

  val regFloat = inFloat.toReg() init()

  outFloat  := regFloat.NormalizedFlow
}

object FpuNormalizerPlay {
  def main(args: Array[String]) {
    SpinalVerilog(SpinalRtlConfig)(new FpuNormalizer(4, 8))
  }
}



