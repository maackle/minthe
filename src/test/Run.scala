package test

import minthe._
import helpers._
import minthe.messages.{PlayNote, Bang}
import units._

object Run {
   def main(args: Array[String]) {

      val ds:List[Double] = List(-256, -255, -128, -127, 0, 127, 128, 255, 256)

      val synth = new Synth

      val osc = {
         val adsr = ADSR(5, 5, 0, 0)
         val adsr2 = (  adsr )
         ( Sine(440) | adsr ) +
         ( Sine(440) | adsr2)
      }

      synth.add(Output(osc))

      synth.start()
      Thread.sleep(1000)

      osc !! Bang
      for(i <- 1 to 10) {
//         dc.set(0 + i/10.0)
         Thread.sleep(1000)
      }

      synth.stop()
   }
}
