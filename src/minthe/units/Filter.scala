package minthe.units

import minthe.Synth._
import minthe.util._

class FilteredSignal(signal:Signal, filter:Filter) extends Composite(signal,filter)(
   (s,f) => {
//      s * math.pow(math.E, (f-1)*8)
      s * math.pow(f, math.E)
   }
) {
   // short-circuit this chain if the filter is off
   override def chunk ={
      if(!filter.active) ZeroChunk
      else super.chunk
   }

}

abstract class Filter extends Signal with Leaf {
   private var _enabled = true
   def active = _enabled
   protected def enable() {
      _enabled = true
   }
   protected def disable() = {
      _enabled = false
      0.0
   }

   def +(that:Filter) = new Composite(this, that)(_+_)
   def -(that:Filter) = new Composite(this, that)(_-_)
}

trait Resettable extends Filter {
   def reset()
   var t0:Double = -9999
}

case class ADSR(a:Double, d:Double, s:Double, r:Double) extends Filter with Resettable {

   disable()

   receive {
      case _ => reset()
   }
   def reset() {
      t0 = clock.seconds.head
      enable()
   }
   @inline def tA = t0 + a
   @inline def tD = tA + d
   @inline def tR = tD + r
   def chunk = {
      clock.seconds map { t =>
         val y = if(t < tA) {
            lerp(0, 1.0)((t - t0)/a)
         }
         else if(t < tD) {
            lerp(1.0, s)((t - tA)/d)
         }
         else if(t < tR) {
            lerp(s, 0.0)((t - tD)/r)
         }
         else {
            disable()
            0.0
         }
         y
      }
   }
}