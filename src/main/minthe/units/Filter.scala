package minthe.units

import minthe.Synth._
import minthe.util._
import test.Settings
import sound.common._
import Settings._

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
//   private var _enabled = true
//   protected def enable() {
//      _enabled = true
//   }
//   protected def disable() = {
//      _enabled = false
//      0.0
//   }
//   def active = _enabled
   def active:Boolean
   def +(that:Filter) = new Composite(this, that)(_+_)
   def -(that:Filter) = new Composite(this, that)(_-_)
}

trait Resettable extends Filter {
   def reset()
   var t0:Double = -9999
}

case class ASDR(var a:Double, var s:Double, var d:Double, var r:Double) extends Filter with Resettable {

   def active = clock.seconds.head < tR

   receive {
      case _ => reset()
   }
   def reset() {
      t0 = clock.seconds.head
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
            0.0
         }
         y
      }
   }
}

case class DC(var level:S) extends Filter with Leaf {

   private var _chunk:Chunk = _

   def active = level != 0.0

   set(level)

   def set(lv:S) {
      level = lv
      _chunk = Array.fill(chunkSize)(level)
      println(level)
   }
   def chunk = _chunk
}

case object Zero extends Filter with Leaf {
   val active = false
   lazy val chunk = throw new Exception("Filter Zero's chunk should never be called")
}