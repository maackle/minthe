package sound

import actors.Actor
import javax.sound.sampled.{AudioSystem, AudioFormat}

class Recorder(af:AudioFormat, chunkSize:Int, subscriber:Actor) extends Actor {
   val bufSize = chunkSize * af.getFrameSize
   val tdl = AudioSystem.getTargetDataLine(af)
   val info = tdl.getLineInfo

   val numBufs = 3
   val bufs:Array[Array[Byte]] = Array.fill(numBufs)(Array.ofDim(bufSize))
   def buf = bufs(ticks%numBufs)

   var stopped = false
   private var ticks = 0

   def stop() {
      tdl.stop()
      tdl.close()
      this ! 'exit
   }

   def act {

      tdl.open(af, bufSize)
      tdl.start()
      this ! 'tick

      while(true) {
         receive {
            case 'tick =>
               tdl.read(buf, 0, bufSize)
               subscriber ! buf            // TODO: is a copy necessary?
               this ! 'tick
            case 'exit => exit()
         }
      }
   }

}