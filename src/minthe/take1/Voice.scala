package menthe.take1

import menthe.Synth
import menthe.Synth._

class Voice(val name: String = "Vox") extends SingleIn {
   def xfer(c: Chunk) = if (on) Identity(c) else ZeroChunk

   private var on = false

   def play() {
      on = true
   }

   def stop() {
      on = false
   }

   override def init(syn: Synth) {
      super.init(syn)
   }

   override def toString = "VOICE[ %s ]".format(name)

}