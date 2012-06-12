package notes


abstract class PitchName(val midiBase:Int, offset:Int=0) {
   override def equals(o:Any) = o match {
      case pn:PitchName => midiBase == pn.midiBase
      case _ => false
   }
   override val hashCode = midiBase

   private def suffix(off:Int) = {
      if(off==0) ""
      else if(off<0) { "b" * (-off) }
      else { "#" * off }
   }

   def apply(octave:Int) = Pitch(this, octave)

   override lazy val toString:String = {
      PitchName.namemap.get(midiBase) match {
         case Some(n) => n
         case _ =>
            val off = if(offset==0) 1 else offset
            val base = (12+midiBase - off)%12
            PitchName.namemap(base) + suffix(off)
      }
   }
}

object PitchName {
   final object G_#  extends PitchName(0,+1)
   final object A_b  extends PitchName(0,-1)
   final object A    extends PitchName(1)
   final object A_#  extends PitchName(2,+1)
   final object B_b  extends PitchName(2,-1)
   final object B    extends PitchName(3)
   final object C    extends PitchName(4)
   final object C_#  extends PitchName(5,+1)
   final object D_b  extends PitchName(5,-1)
   final object D    extends PitchName(6)
   final object D_#  extends PitchName(7,+1)
   final object E_b  extends PitchName(7,-1)
   final object E    extends PitchName(8)
   final object F    extends PitchName(9)
   final object F_#  extends PitchName(10,+1)
   final object G_b  extends PitchName(10,-1)
   final object G    extends PitchName(11)

   val namemap = Map(
      1 -> "A",
      3 -> "B",
      4 -> "C",
      6 -> "D",
      8 -> "E",
      9 -> "F",
      11 -> "G"
   )
   val map = Map(
      0 -> G_#,
      1 -> A,
      2 -> A_#,
      3 -> B,
      4 -> C,
      5 -> C_#,
      6 -> D,
      7 -> D_#,
      8 -> E,
      9 -> F,
      10 -> F_#,
      11 -> G
   )

   def apply(midinum:Int) = {
      map(midinum%12)
   }
}
