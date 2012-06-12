package minthe

import minthe.Synth._
import units._
import Signal._
import sound.common._


case class Mixer(var level:S=0.5) extends Signal with ChunkBuffer with Listener {
   var inputs:List[Signal] = Nil
   def dependents = inputs
   def chunk = {
      inputs map (_.chunk) match {
         case head :: tail =>
            head.copyToArray(buf)
            tail map { chunk =>
               for(i <- 0 until chunk.length) { buf(i) += chunk(i) }
            }
            for(i <- 0 until buf.length) { buf(i) *= level }
            buf.toArray
         case _ => ZeroChunk
      }
   }
   def add(sig:Signal) {
      inputs = sig :: inputs
   }
   def ->: (sig:Signal) = add(sig)
}

case class Voice(sig:Signal) extends Signal with Listener {
   val dependents = sig :: Nil
   def chunk = {
//      Pitcher.chew_fft(sig.chunk)
      sig.chunk
   }
}
