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

package org.scalastuff.proto.value

import scala.collection.mutable.Builder
import com.dyuproject.protostuff.Input
import com.dyuproject.protostuff.Output
import com.dyuproject.protostuff.Pipe

abstract class RepeatedValueHandler(val elementValueHandler: ValueHandler) extends ValueHandler {
  type CB = Builder[elementValueHandler.V, V]

  override val inlined = true

  def defaultValue = {
    val builder = newBuilder()
    builder.result()
  }

  
  def readFrom(input: Input) = throw new UnsupportedOperationException("Cannot deserialize collection of collection, use wrapper")

  def readElementFrom(input: Input) = elementValueHandler.readFrom(input)


  def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
	  elementValueHandler.transfer(tag, pipe, input, output, repeated)
  }

  def newBuilder(): CB
}

abstract class CollectionValueHandler(_elementValueHandler: ValueHandler) extends RepeatedValueHandler(_elementValueHandler) {
  type V = Traversable[elementValueHandler.V]

  override def isDefaultValue(v: V) = v.isEmpty

  def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = {
    require(!repeated, "Collection cannot be serialized inside another collection directly, use wrapper")
    value foreach (elementValueHandler.writeValueTo(tag, output, _, true))
  }
}

abstract class ArrayValueHandler(_elementValueHandler: ValueHandler) extends RepeatedValueHandler(_elementValueHandler) {
  type V = Array[elementValueHandler.V]

  override def isDefaultValue(v: V) = v.length == 0

  def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = {
    require(!repeated, "Array cannot be serialized inside another inlined type directly, use wrapper")
    value foreach (elementValueHandler.writeValueTo(tag, output, _, true))
  }
}