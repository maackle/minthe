package menthe.take1

import menthe.Synth._
import menthe.Synth


class Oscillator(fn:S=>S) extends SingleIn {
   protected override def init(syn:Synth) {
      super.init(syn)
      attach(clock.asInstanceOf[NodeBase])
   }
   def xfer(chunk:Chunk) = chunk map (fn)
   override def output:Chunk = {
      val o = super.output
      o
   }
}

case class Sine(var freq: Double, var gain: Double = 0.1) extends Oscillator((t: S) => gain * math.sin(t * 2 * math.Pi * freq / sampleRate)) {
   override def output: Chunk = {
      val o = super.output
      o
   }

   override def toString = "Sine(%s)".format(freq)
}