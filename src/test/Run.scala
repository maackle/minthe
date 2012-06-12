package test

import minthe._
import minthe.messages.{SetFreq, PlayNote, Bang}
import notes.PitchName.C
import units._

object Run extends App {

   delayedInit {
      simpleTest
      biggerTest
   }

   def simpleTest {
      val synth = new Synth(0.3)
      val osc = Sine(440) + Sine(880)*DC(0.5) + Sine(1320)*DC(0.2) | ASDR(2, 0, 2, 0)

      synth.add( osc )
      synth.start()

      osc !! Bang
      Thread.sleep(5000)

      synth.stop()
   }

   def biggerTest {
      import notes.common._
      import notes.Intervals._
      val synth = new Synth(0.1)
      val root = C(4).midinum

      val tones = List(
         (midi2hz(root + 0)),
         (midi2hz(root + maj3)),
         (midi2hz(root + i5)),
         (midi2hz(root + maj7)),
         (midi2hz(root + i9)),
         (midi2hz(root + i11)),
         (midi2hz(root + i13))
      ) map ( hz => Pulse(hz, 0.5) | ASDR(1.5, 0.3, 5, 0))

      tones foreach { t =>
         synth.add( t )
      }

      synth.start()

      tones foreach { t =>
         t !! Bang
         Thread.sleep(1000)
      }

      Thread.sleep(5000)

      synth.stop()
   }
}
