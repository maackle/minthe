package minthe

import javax.sound.sampled.{AudioFormat, AudioSystem}
import scala.actors.Actor._
import actors.Actor

object Synth {
   type S = Double
   type Chunk = Array[S]

   val sampleRate = 44100
   val chunkSize =  2056
   val ZeroChunk:Chunk = Array.ofDim(chunkSize)

   case object Clock extends Signal {
      tick()
      private var t = 0
      private var rng:Chunk = ZeroChunk
      def tick() {
         rng = Array.range(t, t + chunkSize) map (_.asInstanceOf[S])
         t += chunkSize
      }
      def chunk = { rng }
   }

}

class Synth {
   import Synth._

   def log(a:Any) = println(a)

   private var voices = List[Output]()
   val mix = new Mixer

   def add(v:Output) {
      voices = v :: voices
      v ->: mix
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
         while(true) {
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
                  val bytes = mix.chunk map { s => (s*127).toByte }
                  put(bytes)
                  Clock.tick()
                  if(running) A ! 'tick
               case msg =>
                  throw new Exception("unknown message: " + msg)
            }
         }
      }
   }

   def start() {
      A.start()
      A ! 'start
   }

   def stop() {
      A ! 'stop
   }

}
