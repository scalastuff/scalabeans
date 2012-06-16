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
package value

import com.dyuproject.protostuff.{Pipe, Output, Input}
import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.scalabeans.types._

class WrappedValueHandler(val valueHandler: ValueHandler, valueType: ScalaType) extends ValueHandler {
  type V = valueHandler.V

  val defaultValue = valueHandler.defaultValue

  override def isDefaultValue(v: V) = valueHandler.isDefaultValue(v)

  def writeSchema = readSchema.writeSchema
  val readSchema = BeanBuilderSchema(descriptorOf(TupleType(valueType)))

  def readFrom(input: Input) = {
    val builder = input.mergeObject(null, readSchema)
    val wrapped =  builder.result().asInstanceOf[Tuple1[V]]
    wrapped._1
  }

  def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = {
    val wrapped = Tuple1(value)
    output.writeObject(tag, wrapped, writeSchema, repeated)
  }

  def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
    output.writeObject(tag, pipe, writeSchema.pipeSchema, repeated)
  }
}

object WrappedValueHandler {
  def apply(valueHandler: ValueHandler, valueType: ScalaType) = {
    if (valueHandler.inlined) new WrappedValueHandler(valueHandler, valueType)
    else valueHandler
  }
}