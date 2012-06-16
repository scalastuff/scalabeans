package org.scalastuff.scalabeans.format.proto
import org.scalastuff.util.Format
import org.scalastuff.proto.value.BeanValueHandler
import com.dyuproject.protostuff.ByteArrayInput
import java.io.InputStream
import com.dyuproject.protostuff.CodedInput
import com.dyuproject.protostuff.GraphByteArrayInput
import com.dyuproject.protostuff.GraphCodedInput
import java.io.OutputStream
import com.dyuproject.protostuff.Schema
import com.dyuproject.protostuff.GraphIOUtil
import org.scalastuff.scalabeans.types.ScalaType
import org.scalastuff.scalabeans.types.TupleType
import com.dyuproject.protostuff.ProtostuffOutput
import com.dyuproject.protostuff.GraphProtostuffOutput

class GraphProtostuffFormat[T] private[proto](beanValueHandler: BeanValueHandler) extends Format[T] {
  
  def readFrom(buffer: Array[Byte]): T = {
    val input = new ByteArrayInput(buffer, true)
    val graphInput = new GraphByteArrayInput(input)
    beanValueHandler.beanReadFrom(graphInput).asInstanceOf[T]
  }
  
  def readFrom(inputStream: InputStream): T = {
    val input = new CodedInput(inputStream, true)
    val graphInput = new GraphCodedInput(input)
    beanValueHandler.beanReadFrom(graphInput).asInstanceOf[T]
  }
  
  def writeTo(outputStream: OutputStream, bean: T) = {
    val buffer = BufferPool.getLinkedBuffer()
    GraphIOUtil.writeTo(outputStream, bean, writeSchema, buffer)
  }
  
  def toByteArray(bean: T): Array[Byte] = {
    val buffer = BufferPool.getLinkedBuffer()
    GraphIOUtil.toByteArray(bean, writeSchema, buffer)
  }
  
  private val writeSchema = beanValueHandler.writeSchema.asInstanceOf[Schema[T]]
}

class WrappedGraphProtostuffFormat[T] private[proto](scalaType: ScalaType) extends Format[T] {
  def readFrom(buffer: Array[Byte]): T = wrappedFormat.readFrom(buffer)._1
  
  def readFrom(inputStream: InputStream): T = wrappedFormat.readFrom(inputStream)._1
  
  def writeTo(outputStream: OutputStream, bean: T) = wrappedFormat.writeTo(outputStream, Tuple1(bean))
  
  def toByteArray(bean: T): Array[Byte] = wrappedFormat.toByteArray(Tuple1(bean))
  
  val wrappedSchema = BeanValueHandler(TupleType(scalaType)).writeSchema.asInstanceOf[Schema[Tuple1[T]]]
  
  private val wrappedValueHandler = BeanValueHandler(TupleType(scalaType))
  private val wrappedFormat = new GraphProtostuffFormat[Tuple1[T]](wrappedValueHandler)
}