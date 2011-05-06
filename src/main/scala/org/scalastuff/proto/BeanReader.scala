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

import org.scalastuff.proto.value.BeanValueHandler
import com.dyuproject.protostuff.Input
import java.io.InputStream

class BeanReader[B <: AnyRef](implicit mf: Manifest[B]) {
  private[this] val beanValueHandler = BeanValueHandler[B]()

  def readFrom(input: Input): B = beanValueHandler.beanReadFrom(input).asInstanceOf[B]

  def readFrom(buffer: Array[Byte], format: SerializationFormat) = format.fromByteArray(this, buffer)

  def readFrom(inputStream: InputStream, format: SerializationFormat) = format.readFrom(this, inputStream)

  def messageName = beanValueHandler.writeSchema.messageName
}