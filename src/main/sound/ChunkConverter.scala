package sound

import java.nio.ByteBuffer

class ChunkConverter(bitDepth: Int, signed:Boolean=true, bigEndian:Boolean=true) {
   assert(signed && bigEndian)
   val shortChunks = collection.mutable.Map[Int, Array[Short]]()
   val byteBuffers = collection.mutable.Map[Int, ByteBuffer]()
   val ampMax8 =  (math.pow(2,8)/2-1)
   val ampMax16 = (math.pow(2,16)/2-1)

   //TODO: don't create new array every time
   @inline def toDouble(bytes:Array[Byte]):Array[Double] = {
      val len = bytes.length * 8 / bitDepth
      bitDepth match {
         case 8 =>
            bytes map (_.toDouble / ampMax8)
         case 16 =>
            val buf = shortChunks.getOrElseUpdate(len, Array.ofDim(len))
            val bb = ByteBuffer.wrap(bytes)
            bb.asShortBuffer().get(buf)
            buf map (_.toDouble / ampMax16)
         case _ => throw new Exception("invalid bit depth: %s.  Must be 8 or 16".format(bitDepth))
      }
   }

   @inline def toBytes(ds:Array[Double]):ByteBuffer = {
      val len = ds.length * bitDepth / 8
      val bb = byteBuffers.getOrElseUpdate(len, ByteBuffer.allocate(len))
      bb.rewind()
      bitDepth match {
         case 8 =>
            ds foreach (d => bb.put((d * ampMax8).toByte))
         case 16 =>
            ds foreach (d => bb.putShort((d * ampMax16).toShort))
         case _ => throw new Exception("invalid bit depth: %s.  Must be 8 or 16".format(bitDepth))
      }
      bb.rewind()
      bb
   }


//   @inline def bitify8(s:S) = {
//      Seq(
//         if(fmtSigned)  (s * ampMax).toByte
//         else ((s+1) * ampMax).toByte
//      )
//   }
//   @inline def bitify16(s:S) = {
//      val ss = {
//         (s * ampMax).toInt
//      }
//      Seq((ss>>8).toByte, ss.toByte)
//   }
//   @inline val bitify = {
//      if(bitDepth==8) bitify8 _
//      else if(bitDepth==16) bitify16 _
//      else throw new Exception("unsupported bit depth")
//   }

}