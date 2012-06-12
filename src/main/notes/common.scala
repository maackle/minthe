package notes

package object common {
   val bb = math.pow(2,1.0/12.0)
   def midi2hz(n: Double):Double = (440f * math.pow(bb, n - 49 - 12))
   def hz2midi(f: Double):Double = math.log(f/440.)/math.log(bb) + 49 + 12
   def midi2cent(n:Double):Int = ((((n+0.5)%1.0)-0.5)*100).toInt

}

package object Intervals {
   val i2 = 2
   val min3 = 3
   val maj3 = 4
   val i4 = 5
   val i5 = 7
   val i6 = 9
   val min7 = 10
   val maj7 = 11
   val octave = 12
   val i9 = octave + i2
   val i11 = octave + i4
   val i13 = octave + i6
}
