package minthe.units

import minthe.Synth._
import java.io.PrintWriter
import minthe.messages.Message

import test.Settings
import minthe.Synth
import sound.common._
import Settings._

trait ChunkBuffer {
   val buf:Array[S] = Array.ofDim(chunkSize)
}

object Signal {
   val Pi = math.Pi
   val Pi2 = 2*Pi
   type Xfer = (S)=>S
   val chunkRange = Array.range(0, chunkSize)

   def reduce(a:Chunk, b:Chunk)(fn:(S,S)=>S) = {
      for { i <- chunkRange } yield {
         fn(a(i), b(i))
      }
   }

   val traceDir = "/Users/michael/octave/trace/"

   object Trace {
      def capture() {
         if(!traces.isEmpty) {
            traces map {
               case (lbl, Trace(sig, pw, (t0, t1))) =>
                  val th = Clock.seconds.head
                  if(t0 <= th && th <= t1) {
                     (Clock.seconds zip sig.chunk) map {
                        case(t, s) =>
                           pw.println(t + ", " + s)
                     }
                  }
            }
         }
      }
      def cleanup() {
         println("TRACE CLEANUP:")
         traces map {
            case (lbl, Trace(sig, w, _)) =>
               w.close()
         }
      }
   }

   case class Trace(sig:Signal, pw:PrintWriter, times:(Double, Double)=(0.0, 9999999)) {

   }

   private var traces = Map[String, Trace]()

   def apply(x:S) = { DC(x) }

}


import Signal._

abstract class Signal extends Listener {
   def chunk:Chunk
   def modulate(that:Tone) = { new PMSignal(that, this) }
   val clock = Synth.Clock


   def trace(lbl:String, times:(Double,Double)=(0,999999)) = {
      if(Signal.traces.contains(lbl)) throw new Exception("duplicate trace")
      val path = traceDir + lbl + ".txt"
      Signal.traces = Signal.traces + (lbl -> Trace(this, new PrintWriter(path), times))
      this
   }
   def |(that:Filter) = new FilteredSignal(this, that)
   @deprecated("use filter operator")
   def *(that:Filter) = new Composite(this, that)(_*_)

   def +(that:Signal) = new Composite(this, that)(_+_)
   def -(that:Signal) = new Composite(this, that)(_-_)
   def *(that:Signal) = new Composite(this, that)(_*_)
   def ~(that:Tone) = this.modulate(that)
}


class Composite[T<:Signal](a:T, b:T)(reducer:(S,S)=>S) extends Signal {
   val dependents = a :: b :: Nil
   def chunk = reduce(a.chunk, b.chunk)(reducer)
}


trait HasDependents {
   def dependents:List[Signal]
}

trait Listener extends HasDependents {
   protected def receive(fn:PartialFunction[Message,Unit]) {
      handler = (fn) :: handler
   }

   private var handler:List[PartialFunction[Message,Unit]] = Nil

   final def !(msg:Message) {
      handler map { fn =>
         if(fn.isDefinedAt(msg)) fn(msg)
         for(m <- msg.also) if(fn.isDefinedAt(m)) fn(m)
      }
   }

   final def !!(msg:Message) {
      this ! msg
      dependents.foreach{ d =>
         d !! (msg)
      }
   }
}

trait Leaf {
   val dependents = Nil
}
