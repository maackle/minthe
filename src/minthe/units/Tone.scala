package minthe.units

import minthe.units.Signal._
import minthe.Synth._
import minthe.messages._
import minthe.helpers._


trait Tone extends Signal {
   def freq:Double
   def freq_=(d:Double)
   def fn:Xfer

   def chunk:Chunk = {
      clock.radians map { t => fn(freq * t) }
   }
   val dependents = List[Signal]()

   receive {
      case SetFreq(f) =>
         freq = f
   }
}

case class PMSignal(val original:Tone, val modulator:Signal) extends Tone {
   var freq = original.freq
   def fn:Xfer = original.fn
   override val dependents = List[Signal](original, modulator)

   override def chunk = {
      val t = clock.radians
      val s = clock.seconds
      val mod = modulator.chunk
      val ch = for(i <- chunkRange) yield {
         val o = fn( (t(i) * freq + mod(i) ) )
         o
      }
      ch
   }
}


case class Pulse(var freq:Double, var width:Double) extends Tone {
   private def radwidth = width*Pi2
   def fn = (x) => { if((x%Pi2) < (radwidth)) 1.0 else -1.0 }
   override def toString = "Pulse(%s, %s)".format(freq, width)
}

case class Sine(var freq: Double) extends Tone {
   def fn = math.cos _
   override def toString = "Sine(%s)".format(freq)
}

case class Saw(var freq:Double) extends Tone {
   import math.Pi
   def fn = t => (Pi - (t%(2*Pi))) / Pi
   override def toString = "Saw(%s)".format(freq)
}
case class Triangle(var freq:Double) extends Tone {
   import math.Pi
   def fn = t => {
      val a = (t)%(2*Pi)
      if(a < Pi) (2*a - Pi) / Pi
      else (3*Pi - 2*a) / Pi
   }
   override def toString = "Triangle(%s)".format(freq)
}
