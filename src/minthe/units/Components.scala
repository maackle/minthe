package minthe

import minthe.Synth._
import units._
import Signal._


case class Mixer() extends Signal with ChunkBuffer with Listener {
   var inputs:List[Signal] = Nil
   def dependents = inputs
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

case class Output(sig:Signal) extends Signal with Listener {
   val dependents = sig :: Nil
   def chunk = sig.chunk
}
