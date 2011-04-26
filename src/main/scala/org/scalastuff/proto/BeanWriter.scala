/*
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

import org.scalastuff.proto.value.BeanValueHandler
import com.dyuproject.protostuff._
import java.io.OutputStream

class BeanWriter[B <: AnyRef](implicit mf: Manifest[B]) {

  def writeTo(output: Output, bean:B) {
    beanValueHandler.beanWriteTo(output, bean)
  }

  def writeTo(outputStream: OutputStream, bean: B, format: SerializationFormat) = format.writeTo(this, bean, outputStream)
  
  def toByteArray(bean: B, format: SerializationFormat) = format.toByteArray(this, bean)

  def schema = beanValueHandler.writeSchema.asInstanceOf[Schema[AnyRef]]
  private[this] val beanValueHandler = BeanValueHandler[B]()
}