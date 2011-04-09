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

import com.dyuproject.protostuff.{Pipe, Input, Output}

class OptionValueHandler(val valueHandler: ValueHandler) extends ValueHandler {
  type V = Option[valueHandler.V]

  override val inlined = true

  val defaultValue = None

  override def isDefaultValue(v: V) = v.isEmpty

  def readFrom(input: Input) = Some(valueHandler.readFrom(input))

  def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = {
    require(!repeated, "Option cannot be serialized inside collection directly, use wrapper")
    value foreach (valueHandler.writeValueTo(tag, output, _, repeated))
  }

  def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
    valueHandler.transfer(tag, pipe, input, output, repeated)
  }
}