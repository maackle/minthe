package minthe

import collection.mutable.ArrayBuffer
import minthe.Synth._


trait ChunkBuffer {
   val buf:Array[S] = Array.ofDim(chunkSize)
}

object Signal {
   type Xfer = (S)=>S
   val chunkRange = Array.range(0, chunkSize)

   def reduce(a:Chunk, b:Chunk)(fn:(S,S)=>S) = {
      for { i <- chunkRange } yield {
         fn(a(i), b(i))
      }
   }

   def trace(chunk:Chunk, lbl:String="") = {
      println(chunk mkString("%s: ".format(lbl)," ",""))
      chunk
   }
   def trace(signal:Signal) = {
      println(signal.chunk mkString("SIG %s : ".format(signal)," ",""))
      signal
   }

   def apply(x:S) = { SteadySignal(x) }

}
import Signal._

abstract class Signal {
   def chunk:Chunk
   def modulate(that:Tone) = {
      val self = this
      new ControlledTone {
         val gain = Signal(that.gain)
         def fn = that.fn
         val freq = self + Signal(that.freq)
//         val freq = Signal(550) + Signal(2)
      }
   }
   def +(that:Signal) = new SignalCombo(this, that)(_+_)

//   def +(that:Signal) = new Signal {
//      def chunk = {
//         for { i <- chunkRange } { scratch(i) = this.chunk(i) + that.chunk(i) }
//         scratch
//      }
//   }
   def *(that:Signal) = new SignalPlus(this, that)
   def ^(that:Tone) = this.modulate(that)
}

case class SteadySignal(x:S) extends Signal { val chunk = Array.fill(chunkSize)(x) }

class SignalCombo(a:Signal, b:Signal)(reducer:(S,S)=>S) extends Signal with ChunkBuffer {
   def chunk = reduce(a.chunk, b.chunk)(reducer)
}

class SignalPlus(a:Signal, b:Signal) extends SignalCombo(a,b)(null) {
   //   def chunk = reduce(a.chunk, b.chunk)(reducer)
   override def chunk = {
      for { i <- chunkRange } { buf(i) = a.chunk(i) + b.chunk(i) }
      buf
   }
}
//
//class CompoundSignal extends Signal with ChunkBuffer {
//
//}

abstract class ControlledTone extends Signal {
   val freq:Signal
   val gain:Signal
   val source:Signal = Synth.Clock
   def fn:Xfer
//   def freq_=(f:Double) { chunkRange foreach (freq(_) = f) }
//   private def angle = freq.chunk map { f => f * 2 * math.Pi / sampleRate }
   def generator = (x:Chunk) => for(i <- chunkRange) yield {
      gain.chunk(i) *
         fn(
            x(i) *
               freq.chunk(i) *
               2 *
               math.Pi / sampleRate
         )
   }
   def chunk = {
      val x = source.chunk
      val f = freq.chunk
      val g = gain.chunk
      for(i <- chunkRange) yield {
         g(i) *
            fn(
               x(i) *
                  f(i) *
                  2 *
                  math.Pi / sampleRate
            )
      }
   }
}

trait Tone extends Signal {
   def freq:Double
   def gain:Double
   protected def angle = freq * 2 * math.Pi / sampleRate
   def fn:Xfer
   def generator = (x:S) => gain * fn(angle * x)
   def chunk:Chunk = {
      Synth.Clock.chunk map generator
   }

}

case class Sine(var freq: Double, var gain: Double = 0.1) extends Tone {
   def fn = math.sin _
   override def toString = "Sine(%s)".format(freq)
}

case class Mixer() extends Signal with ChunkBuffer {
   var inputs:List[Signal] = Nil
   def chunk = {
      inputs map (_.chunk) match {
         case head :: tail =>
            head.copyToArray(buf)
            tail map { chunk =>
               for(i <- 0 until chunk.length) { buf(i) += chunk(i) }
            }
            buf.toArray
         case _ => ZeroChunk
      }
   }
   def add(sig:Signal) {
      inputs = sig :: inputs
   }
   def ->: (sig:Signal) = add(sig)
}

case class Output(val sig:Signal) extends Signal {
   def chunk = sig.chunk
}
