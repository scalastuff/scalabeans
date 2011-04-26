package org.scalastuff.proto

import com.dyuproject.protostuff.{LinkedBuffer, Output, GraphProtostuffOutput, ProtobufOutput, ProtostuffOutput}
import org.scalastuff.proto.value.BeanValueHandler

class BeanWriter[B <: AnyRef](implicit mf: Manifest[B]) {
  import BeanWriter._
  
  def writeTo(output: Output, bean:B) {
    beanValueHandler.beanWriteTo(output, bean)
  }
  
  def toProtobufByteArray(bean: B) = {
    val linkedBuffer = getLinkedBuffer()
    val output = new ProtobufOutput(linkedBuffer)
    writeTo(output, bean)
    output.toByteArray()
  }
  
  def toGraphProtostuffByteArray(bean: B) = {
    val linkedBuffer = getLinkedBuffer()
    val output = new ProtostuffOutput(linkedBuffer) 
    val graphOutput = new GraphProtostuffOutput(output)
    writeTo(output, bean)
    output.toByteArray()
  }
  
  private[this] val beanValueHandler = BeanValueHandler[B]()
}

object BeanWriter {
  val linkedBufferPool = new ThreadLocal[LinkedBuffer] {
    override protected def initialValue() = LinkedBuffer.allocate(512)
  }
  
  def getLinkedBuffer() = {
    val linkedBuffer = linkedBufferPool.get
    linkedBuffer.clear()
    linkedBuffer
  }
}