package minthe

import javax.sound.sampled.{AudioFormat, AudioSystem}
import actors.Actor
import java.io.PrintWriter
import collection.mutable.ListBuffer
import units._
import sound.ChunkConverter
import test.Settings
import sound.common._
import Settings._

object Synth {

   val ZeroChunk: Chunk = Array.fill(chunkSize)(0.0)

   val logg = ListBuffer[String]()
   val samplog = ListBuffer[Byte]()
   val pw = new PrintWriter("/Users/michael/octave/wave.txt")

   trait Clock

   case object Clock {
      tick()
      private var _ticks = 0
      def ticks = _ticks
      private var t = 0
      private var ts, rads, secs = ZeroChunk
      def tick() {
         ts = Array.range(t, t + chunkSize) map { t => (t).asInstanceOf[S] }
         secs = ts map { t => t / sampleRate }
         rads = ts map { t =>
            (t * 2 * math.Pi / sampleRate) // don't do mod here!
         }
         t += chunkSize
         _ticks += 1
      }
      def counts  = ts
      def radians = rads
      def seconds = secs
      object Signals {
         def radians = new Signal with Leaf { def chunk = rads }
         def seconds = new Signal with Leaf { def chunk = secs }
      }
   }

}

class Synth(gain:Double) {
   import Synth._

   def log(a:Any) = println(a)

   private var voices = List[Voice]()
   val mix = new Mixer(gain)

   val chunkRange = Array.range(0, chunkSize)

   def add(s:Signal) {
      add(Voice(s))
   }

   def add(v:Voice) {
      voices = v :: voices
      v ->: mix
      log("added %s".format(v.toString))
   }

   case class ByteLoad(bytes:Array[Byte])
   case class Compute(a:Actor)

   object A extends Actor {

      val af = new AudioFormat(sampleRate, bitDepth, 1, fmtSigned_?, fmtBigEndian_?)
      val sdl = AudioSystem.getSourceDataLine(af)
      log(af)

      var running = false

      @inline def put(bytes: Array[Byte]) {
         sdl.write(bytes, 0, bytes.length)
      }

      def act {

         sdl.open(af, chunkSize)
         sdl.start()
         running = true

         while(true) {
            receive {
               case 'start =>
                  log("start")
                  Computer ! Compute(this)
               case 'stop =>
                  log("stop")
                  sdl.drain()
                  sdl.stop()
                  sdl.close()
                  running = false
                  exit()
               case ByteLoad(bytes) if(running) =>
                  Computer ! Compute(this)
                  put(bytes)
               case msg =>
                  throw new Exception("unknown message: " + msg)
            }
         }
      }
   }

   object Computer extends Actor {

      val ampMax = math.pow(2,bitDepth)/2-1

      val numBufs = 2
      var bufs:Array[Array[Byte]] = Array.fill(numBufs)(Array.fill(chunkSize * byteDepth)(0.toByte) )

      val cc = new ChunkConverter(bitDepth)

      def act {
         loop {
            react {
               case Compute(actor) =>
                  val buf = bufs(Clock.ticks % numBufs)
                  val bb = cc.toBytes(mix.chunk)
                  bb.get(buf)
                  Clock.tick()
                  actor ! ByteLoad(buf)
               case 'stop =>
                  exit()
            }
         }
      }
   }

   def start() {
      A.start()
      Computer.start()
      A ! 'start
   }

   def stop() {
      samplog.map {line => pw.println(line)}
      pw.close()
      Signal.Trace.cleanup()
      A ! 'stop
      Computer ! 'stop
   }

}
