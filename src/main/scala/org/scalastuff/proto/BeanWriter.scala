/**
 * Copyright (c) 2011 ScalaStuff.org (joint venture of Alexander Dvorkovyy and Ruud Diterwich)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package org.scalastuff.proto

import java.io.OutputStream
import org.scalastuff.proto.value._
import com.dyuproject.protostuff.Schema
import com.dyuproject.protostuff.Output
import org.scalastuff.scalabeans.types.ScalaType

/**
 * Writes values of given type to OutputStream or byte array.
 * 
 * Use [[org.scalastuff.proto.Preamble#writerOf]] to get the right instance implementing this interface.
 * 
 * @see [[org.scalastuff.proto.SerializationFormat]]
 */
trait Writer[B] {
  def writeTo(output: Output, bean: B): Unit
  def writeTo(outputStream: OutputStream, obj: B, format: SerializationFormat): Unit
  def toByteArray(obj: B, format: SerializationFormat): Array[Byte]
  def schema: Schema[AnyRef]
}

/**
 * Implements Writer interface for given bean type
 *  
 * @see [[org.scalastuff.proto.Writer]]
 */
class BeanWriter[B <: AnyRef: Manifest] extends Writer[B] {

  def writeTo(output: Output, bean: B) {
    beanValueHandler.beanWriteTo(output, bean)
  }

  def writeTo(outputStream: OutputStream, bean: B, format: SerializationFormat) = format.writeTo(this, bean, outputStream)

  def toByteArray(bean: B, format: SerializationFormat) = format.toByteArray(this, bean)

  def schema = beanValueHandler.writeSchema.asInstanceOf[Schema[AnyRef]]
  
  private[this] val beanValueHandler = BeanValueHandler[B]()
}

/**
 * Implements Writer interface by delegating all method calls to BeanWriter[Tuple1[A]].
 * 
 * @see [[org.scalastuff.proto.Writer]]
 */
class WrappedWriter[A: Manifest] extends Writer[A] {
  def writeTo(output: Output, obj: A) = wrapped.writeTo(output, Tuple1(obj))
  
  def writeTo(outputStream: OutputStream, obj: A, format: SerializationFormat) = wrapped.writeTo(outputStream, Tuple1(obj), format)
  
  def toByteArray(obj: A, format: SerializationFormat) = wrapped.toByteArray(Tuple1(obj), format)
  
  def schema = wrapped.schema

  private[this] val wrapped = new BeanWriter[Tuple1[A]]
}