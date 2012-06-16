package org.scalastuff.scalabeans.format.proto
import com.dyuproject.protostuff.LinkedBuffer

object BufferPool {
  private[this] val linkedBufferPool = new ThreadLocal[LinkedBuffer] {
    override protected def initialValue() = LinkedBuffer.allocate(512)
  }

  def getLinkedBuffer() = {
    val linkedBuffer = linkedBufferPool.get
    linkedBuffer.clear()
    linkedBuffer
  }
}