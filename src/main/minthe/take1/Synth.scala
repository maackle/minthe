package menthe.take1

import scala.actors.Actor._
import actors.Actor
import javax.sound.sampled.{AudioFormat, AudioSystem}
import menthe.Mixer

object Synth {
   type S = Double
   type Chunk = Array[S]

   //TODO: move these to class, add Synth reference to all component classes
   val sampleRate = 44100
   val chunkSize =  2056
   val ZeroChunk:Chunk = Array.ofDim(chunkSize)

   def midi2hz(n: Int): Int = (440f * math.pow((math.pow(2, 1 / 12f)), n - 49 - 12)).toInt

}

class Synth {

   import Synth._

   A.start()

   object Clock extends NodeBase {
      tick()
      private var t = 0
      private var rng: Array[S] = ZeroChunk

      def tick() {
         rng = Array.range(t, t + chunkSize) map (_.asInstanceOf[S])
         t += chunkSize
      }

      def attach(other: NodeBase) = ()

      def incoming = null

      def xfer = null

      override def output = {
         rng
      }
   }

   def log(a: Any) = println(a)

   private var voices = List[Voice]()
   val mix = new Mixer

   def add(v: Voice) {
      voices = v :: voices
      v ~> mix
      v.init(this)
      log("added %s".format(v.toString))
   }

   object A extends Actor {

      val SAMPLE_RATE = 41000f
      val BYTE_BUFFER_SIZE = 1024
      val buf: Array[Byte] = Array.ofDim(BYTE_BUFFER_SIZE)
      val af = new AudioFormat(SAMPLE_RATE, 8, 1, true, false)
      val sdl = AudioSystem.getSourceDataLine(af)

      var running = false

      def put(bytes: Array[Byte]) {
         sdl.write(bytes, 0, bytes.length)
      }

      def act {
         while (true) {
            receive {
               case 'start =>
                  log("start")
                  sdl.open(af, chunkSize)
                  sdl.start()
                  running = true
                  A ! 'tick
               case 'stop =>
                  log("stop")
                  sdl.drain()
                  sdl.stop()
                  sdl.close()
                  running = false
                  exit()
               case 'tick =>
                  val bytes = mix.output map {
                     s => (s * 127).toByte
                  }
                  put(bytes)
                  Clock.tick()
                  if (running) A ! 'tick
               case msg =>
                  throw new Exception("unknown message: " + msg)
            }
         }
      }
   }

   def start() {
      A ! 'start
   }

   def stop() {
      A ! 'stop
   }

}
