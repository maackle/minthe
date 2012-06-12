package test

import javax.sound.sampled.AudioFormat
import sound.common._

object Settings {

   val sampleRate = 44100 / 4
   val chunkSize = math.pow(2, 12).toInt
   val byteDepth = 2
   val bitDepth = byteDepth * 8
   val fmtSigned_? = true
   val fmtBigEndian_? = true
   val ampMax = math.pow(2, bitDepth) / 2 - 1
   val af = new AudioFormat(sampleRate, bitDepth, 1, fmtSigned_?, fmtBigEndian_?)
}
