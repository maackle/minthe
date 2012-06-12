package test

object Timer {
   def apply(label:String)(block: =>Unit) {
      val t = compat.Platform.currentTime
      block
      println("%s: %d ms".format(label, compat.Platform.currentTime - t))
   }
}
