package minthe


package object util {

   def lerp[T : Numeric] (a:(T), b:(T))(t:Double, p:Double=1.0):Double = {
      val imp = implicitly[Numeric[T]]
      val fa = imp.toFloat(a)
      val u = if(p==1.0) t else math.pow(t,p).toFloat
      fa + (imp.toFloat(b)-fa) * u
   }
}

package object messages {
   import notes.common._

   trait Message {
      val also:List[Message] = Nil
   }

   case object Bang extends Message

   case class SetFreq(freq:Double) extends Message

   case class PlayNote(midinum:Double) extends Message {
      override val also = SetFreq(midi2hz(midinum)) :: Bang :: Nil
   }
}