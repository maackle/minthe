package notes

import common._

case class Pitch(name:PitchName, octave:Int, cent:Int=0) {
   def midinum = name.midiBase + 12*octave
   def hz = midi2hz(midinum)
   override val toString = name.toString + octave + {
      if(cent==0) "" else " %+2d%%".format(cent)
   }
}

object Pitch {
   def fromHz(f:Double) = fromMidi(hz2midi(f))
   def fromMidi(m:Double) = Pitch(PitchName((m).round.toInt), m.toInt/12, midi2cent(m))
}
