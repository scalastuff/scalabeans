package org.scalastuff.scalabeans.format.proto
import java.io.InputStream
import java.io.OutputStream

import org.scalastuff.proto.value.BeanValueHandler
import org.scalastuff.scalabeans.Preamble.descriptorOf
import org.scalastuff.scalabeans.types.TupleType
import org.scalastuff.scalabeans.Metamodel
import org.scalastuff.util.Format

import com.dyuproject.protostuff.Schema
import com.dyuproject.protostuff.ByteArrayInput
import com.dyuproject.protostuff.CodedInput
import com.dyuproject.protostuff.ProtobufIOUtil


class ProtobufFormat[T] private[proto](beanValueHandler: BeanValueHandler) extends Format[T] {
  
  def readFrom(buffer: Array[Byte]): T = {
    val input = new ByteArrayInput(buffer, false)
    beanValueHandler.beanReadFrom(input).asInstanceOf[T]
  }
  
  def readFrom(inputStream: InputStream): T = {
    val input = new CodedInput(inputStream, false)
    beanValueHandler.beanReadFrom(input).asInstanceOf[T]
  }
  
  def writeTo(outputStream: OutputStream, bean: T) = {
    val buffer = BufferPool.getLinkedBuffer()
    ProtobufIOUtil.writeTo(outputStream, bean, writeSchema, buffer)
  }
  
  def toByteArray(bean: T): Array[Byte] = {
    val buffer = BufferPool.getLinkedBuffer()
    ProtobufIOUtil.toByteArray(bean, writeSchema, buffer)
  }
  
  private val writeSchema = beanValueHandler.writeSchema.asInstanceOf[Schema[T]]
}

class WrappedProtobufFormat[T] private[proto](metamodel: Metamodel) extends Format[T] {
  def readFrom(buffer: Array[Byte]): T = wrappedFormat.readFrom(buffer)._1
  
  def readFrom(inputStream: InputStream): T = wrappedFormat.readFrom(inputStream)._1
  
  def writeTo(outputStream: OutputStream, bean: T) = wrappedFormat.writeTo(outputStream, Tuple1(bean))
  
  def toByteArray(bean: T): Array[Byte] = wrappedFormat.toByteArray(Tuple1(bean))
  
  val wrappedSchema = BeanValueHandler(descriptorOf(TupleType(metamodel.scalaType))).writeSchema.asInstanceOf[Schema[Tuple1[T]]]
  
  private val wrappedValueHandler = BeanValueHandler(descriptorOf(TupleType(metamodel.scalaType)))
  private val wrappedFormat = new ProtobufFormat[Tuple1[T]](wrappedValueHandler)
}