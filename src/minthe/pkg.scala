package minthe

package object helpers {

   def midi2hz(n: Int): Int = (440f * math.pow((math.pow(2, 1 / 12f)), n - 49 - 12)).toInt

}
