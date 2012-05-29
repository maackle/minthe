package test

import minthe._
import helpers._

object Run {
   def main(args: Array[String]) {
      val synth = new Synth

      val osc = Sine(440) + (Sine(1, 10) ^ Sine(880)) + Sine(1200)

//      val osc = (
//         Sine(midi2hz(60)) + Sine(midi2hz(64)) + Sine(midi2hz(67)) + Sine(midi2hz(71)) + Sine(midi2hz(74)) + Sine(midi2hz(78))
//      ) ^ Sine(440, 0.5)

      synth.add(Output(osc))

      synth.start()

      Thread.sleep(10000)

      synth.stop()
   }
}
