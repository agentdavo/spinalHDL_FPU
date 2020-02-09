package fpu

import spinal.core._
import spinal.lib._

class FpuAdd(config: FpuConfig) extends Component {
  import config._

  val io = new Bundle {
    val u1 = in (Floating(exponentSize, mantissaSize)),
    val u2 = in (Floating(exponentSize, mantissaSize)),
    val y = out (Floating(exponentSize, mantissaSize + 4)),
    val start = in Bool,
    val done = out Bool
  }

  val fsm = new StateMachine{
    val small = Reg(Floating(exponentSize, mantissaSize)) init (0)
    val big = Reg(Floating(exponentSize, mantissaSize)) init (0)
    val smallIsDenormalized = small.isExponentZero
    val bigIsDenormalized = big.isExponentZero
    val bigNormSmallDenorm = smallIsDenormalized && !bigIsDenormalized
    val exponentDiff = (big.exponent - small.exponent - bigNormSmallDenorm)

    val bigAdd = Reg(Bits(mantissaSize + 4)) init (0)
    val smallAdd = Reg(Bits(mantissaSize + 4)) init (0)
    val smallShift = Reg(Bits(mantissaSize + 4)) init (0)
    val sum = Reg(Bits(mantissaSize + 4)) init (0)
    val sum_2 = Reg(Bits(mantissaSize + 4)) init (0)
    val sumOverflow = sum(mantissaSize + 4 - 1)

    val smallIsNonZero = small.exponent.orR | small.mantissa.orR
    val smallShiftNonzero = smallShift.orR
    val smallFractionEnable = smallIsNonZero && !smallShiftNonzero
    val denormToNorm = sum_2(mantissaSize + 4 - 1)
    val exponent = Bits(exponentSize)
    val exponent_2 = Bits(exponentSize)
    val done = Bool

    io.done := done
    io.y.sign := big.sign
    io.y.mantissa := sum_2
    io.y.exponent = exponent_2

    val START : State = new State with EntryPoint {
      whenIsActive {
        done  := False;
        when (io.start) {
          goto(COMPARE)
        }
      }
    }

    val COMPARE : State = new State {
      whenIsActive {
        (small, big) := (u1.exponent > u2.exponent) ? (u2, u1) | (u1, u2)
        goto(ADD_1)
      }
    }

    val ADD_1 : State = new State{
      whenIsActive {
        bigAdd := (B(0, 1 bit) ## !smallIsDenormalized ## small.mantissa ## B(0, 2 bit))
        smallAdd := (B(0, 1 bit) ## !smallIsDenormalized ## small.mantissa ## B(0, 2 bit))
        smallShift := smallAdd >> exponentDiff 
        goto(ADD_2)
      }
    }

    val ADD_2 : State = new State{
      whenIsActive {
        sum := bigAdd + (smallFractionEnable === 1) ? B(1, 56 bit) | smallShift
        goto(ADD_3)
      }
    }

    val ADD_3 : State = new State{
      whenIsActive {
        sum_2 := (sumOverflow === 1) ? (sum >> 1) | (sum)
        exponent := (sumOverflow === 1) ? (large.exponent + 1) | (large.exponent)
        goto(stateA)
      }
    }

    val ADD_4 : State = new State{
      whenIsActive {
        exponent_2 := (denormToNorm === 1) ? (exponent + 1) | (exponent)
        goto(DONE)
      }
    }

    val DONE : State = new State{
      onEntry(done := True)
      whenIsActive (goto(START))
    }
  }

}



