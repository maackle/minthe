package sound

package object common {
   type S = Double
   type Chunk = Array[S]

   def RMS(chunk:Chunk) = {
      math.sqrt(
         chunk.map {
            v => v * v
         }.reduce(_ + _) / chunk.length
      )
   }

}
