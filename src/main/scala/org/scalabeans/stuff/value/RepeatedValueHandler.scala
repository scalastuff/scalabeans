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

package org.scalabeans.stuff.value

import scala.collection.mutable.Builder
import com.dyuproject.protostuff.Input
import com.dyuproject.protostuff.Output
import com.dyuproject.protostuff.Pipe

abstract class RepeatedValueHandler(val elementValueHandler: ValueHandler) extends ValueHandler {
  type V = Traversable[elementValueHandler.V]
  type CB = Builder[elementValueHandler.V, V]

  override val inlined = true

  def defaultValue = {
    val builder = newBuilder()
    builder.result()
  }
  override def isDefaultValue(v: V) = v.isEmpty
  
  def readFrom(input: Input) = throw new UnsupportedOperationException("Cannot deserialize collection of collection, use wrapper")

  def readElementFrom(input: Input) = elementValueHandler.readFrom(input)

  def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = {
    require(!repeated, "Collection cannot be serialized inside another collection directly, use wrapper")
    value foreach (elementValueHandler.writeValueTo(tag, output, _, true))
  }

  def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
	  elementValueHandler.transfer(tag, pipe, input, output, repeated)
  }

  def newBuilder(): CB
}