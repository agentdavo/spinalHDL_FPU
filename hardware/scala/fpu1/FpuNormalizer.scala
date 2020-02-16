package fpu
import spinal.core._
import spinal.lib._

class FpuNormalizer(exponentSize: Int,
                    mantissaSize: Int,
                    exponentBias: Int) extends Component {
  val inFloat = slave Flow (FloatData(exponentSize, mantissaSize, exponentBias))
  val outFloat = master Flow (FloatData(exponentSize, mantissaSize, exponentBias))

  val regFloat = inFloat.toReg()

  outFloat  := regFloat.NormalizedFlow
}

object FpuNormalizerPlay {
  def main(args: Array[String]) {
    SpinalVerilog(SpinalRtlConfig)(new FpuNormalizer(4, 8, 7))
  }
}



