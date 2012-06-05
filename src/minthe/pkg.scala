package minthe

import minthe.helpers._

package object util {

   def lerp[T : Numeric] (a:(T), b:(T))(t:Double, p:Double=1.0):Double = {
      val imp = implicitly[Numeric[T]]
      val fa = imp.toFloat(a)
      val u = if(p==1.0) t else math.pow(t,p).toFloat
      fa + (imp.toFloat(b)-fa) * u
   }
}
object messages {

   trait Message {
      val also:List[Message] = Nil
   }

   case object Bang extends Message

   case class SetFreq(freq:Double) extends Message

   case class PlayNote(midinum:Double) extends Message {
      override val also = SetFreq(midi2hz(midinum)) :: Bang :: Nil
   }
}
package object helpers {

   def midi2hz(n: Double): Double = (440f * math.pow((math.pow(2, 1 / 12f)), n - 49 - 12))

}

package object intervals {

   val i2 = 2
   val min3 = 3
   val maj3 = 4
   val i4 = 5
   val i5 = 7
   val i6 = 9
   val min7 = 10
   val maj7 = 11
   val octave = 12
   val i9 = octave + i2
   val i11 = octave + i4
   val i13 = octave + i6
}