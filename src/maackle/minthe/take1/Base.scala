package menthe.take1

import menthe.Synth
import Synth.{S, chunkSize, Chunk, ZeroChunk}

trait ChunkBuffer {
   val buf: Array[S] = Array.ofDim(chunkSize)
}

trait NodeBase {
   protected def attach(other: NodeBase)

   private var _synth: Synth = null

   protected def synth: Synth = {
      if (_synth == null) throw new Exception("Failed to initialize synth component")
      else _synth
   }

   protected def synth_=(syn: Synth) {
      _synth = syn
   }

   protected var input: List[NodeBase] = Nil

   def output: Chunk

   def clock = synth.Clock

   def ~>(c: NodeBase): NodeBase = {
      c.attach(this)
      c
   }

   def makeVoice = {
      val v = new Voice
      this ~> v
      v
   }

   protected def init(syn: Synth) {
      synth = syn
      println("init")
      input map {
         n => n.init(syn)
      }
   }

   def pr(a: Any) = println(a)

   protected def traverseRootwise(fn: (NodeBase) => Unit) {
      fn(this)
      input map {
         n => n.traverseRootwise(fn)
      }
   }
}


trait SingleIn extends NodeBase {
   //   protected var input:Option[NodeBase] = None
   def Identity(c: Chunk) = c

   def xfer(c: Chunk): Chunk

   def attach(other: NodeBase) {
      input = List(other)
   }

   def output = input match {
      case List(in) => xfer(in.output)
      case Nil => ZeroChunk
   }
}

trait MultiIn extends NodeBase {
   //   protected var input = List[NodeBase]()
   def attach(other: NodeBase) {
      input = other :: input
   }

   def xfer: List[Chunk] => Chunk

   def output: Chunk = {
      val ch = input map {
         in =>
            in.output
      }
      xfer(ch)
   }
}

