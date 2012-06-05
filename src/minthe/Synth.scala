package minthe

import javax.sound.sampled.{AudioFormat, AudioSystem}
import scala.actors.Actor._
import actors.Actor
import collection.SeqView
import java.io.PrintWriter
import java.nio.ByteBuffer
import collection.mutable.{Buffer, ListBuffer, IndexedSeqView}
import units._

object Synth {
   type S = Double
   type Chunk = Array[S]

   val sampleRate = 44100
   val chunkSize = math.pow(2,12).toInt
   val byteDepth = 2
   val fmtSigned = true
   val fmtBigEndian = false
   val ZeroChunk:Chunk = Array.ofDim(chunkSize)
   val bitDepth = byteDepth * 8

   val logg = ListBuffer[String]()
   val samplog = ListBuffer[Byte]()
   val pw = new PrintWriter("/Users/michael/octave/wave.txt")

   trait Clock

   case object Clock {
      tick()
      private var t = 0
      private var ts, rads, secs = ZeroChunk
      def tick() {
         ts = Array.range(t, t + chunkSize) map { t => (t).asInstanceOf[S] }
         secs = ts map { t => t / sampleRate }
         rads = ts map { t =>
            (t * 2 * math.Pi / sampleRate) // don't do mod here!
         }
         t += chunkSize
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

class Synth {
   import Synth._

   def log(a:Any) = println(a)

   private var voices = List[Output]()
   val mix = new Mixer

   var sharedByteBuf:Array[Byte] = Array.fill(chunkSize * byteDepth)(0.toByte)
   val chunkRange = Array.range(0, chunkSize)

   def add(v:Output) {
      voices = v :: voices
      v ->: mix
      log("added %s".format(v.toString))
   }

   case class ByteLoad(bytes:Array[Byte])
   case class Compute(a:Actor)

   object A extends Actor {

      val af = new AudioFormat(sampleRate, bitDepth, 1, fmtSigned, fmtBigEndian)
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
               case ByteLoad if(running) =>
                  Computer ! Compute(this)
                  put(sharedByteBuf)
               case msg =>
                  throw new Exception("unknown message: " + msg)
            }
         }
      }
   }

   object Computer extends Actor {

      val ampMax = math.pow(2,bitDepth)/2-1
      @inline def bitify8(s:S) = {
         Seq(
            if(fmtSigned)  (s * ampMax).toByte
            else ((s+1) * ampMax).toByte
         )
      }
      @inline def bitify16(s:S) = {
         val ss = {
            (s * ampMax).toInt
         }
         Seq(ss.toByte, (ss>>8).toByte)
      }
      @inline val bitify = {
         if(bitDepth==8) bitify8 _
         else if(bitDepth==16) bitify16 _
         else throw new Exception("unsupported bit depth")
      }

      @inline def chunk2bytes(chunk:Chunk) {
         if(byteDepth==1) { // 8 bit
            throw new Exception("not implemented")
         }
         else if(byteDepth == 2) { // 16 bit
            for(i <- chunkRange) {
               val ss = (chunk(i) * ampMax).toInt
               sharedByteBuf(i<<1) = ss.toByte
               sharedByteBuf((i<<1)+1) = (ss>>8).toByte
            }
         }
         else throw new Exception("unsupported bit depth")
      }

      def act {
         while(true) {
            receive {
               case Compute(actor) =>
                  sharedByteBuf = mix.chunk flatMap bitify
                  Clock.tick()
//                  chunk2bytes(mix.chunk)
//                  bytes map (samplog += _)
//                  Signal.Trace.capture()
                  actor ! ByteLoad
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
