import spinal.core._
import spinal.lib._

object Utils {
  def count_zeros_mul (data: Bits): Bits {
    val count = Bits(log2up(mantissaSize) bits)
    for (idx <- (2 * mantissaSize - 1) downto mantissaSize) {
      switch (data(idx)) {
        is(B"0") { 
          count := count + 1
        }
        is(B"1") {
          break
        }
      }
    }
    count
  }

  def count_1_zeros (data: Bits): Bits {
    val count = Bits(log2up(data) bits)
    for (dataBit <- data) {
      switch (dataBit) {
        is(B"0") { 
          count := count + 1
        }
        is(B"1") {
          break
        }
      }
    }
    count
  }
}



