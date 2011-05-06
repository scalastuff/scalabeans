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

import com.dyuproject.protostuff._
import value.ValueHandler
import org.scalastuff.scalabeans.PropertyDescriptor

abstract class Field[B <: AnyRef](val tag: Int, propertyDescriptor: PropertyDescriptor) {

  val valueHandler: ValueHandler

  val repeated: Boolean = false

  def name = propertyDescriptor.name

  def writeTo(output: Output, message: B) {
    val value = getNotNullValue(message)
    if (!valueHandler.isDefaultValue(value))
      valueHandler.writeValueTo(tag, output, value, false)
  }

  def transfer(pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
    valueHandler.transfer(tag, pipe, input, output, repeated)
  }

  private def getNotNullValue(message: B) = {
    val value = getValue(message)
    if (value == null)
      throw new UninitializedFieldError("Cannot write %s to output: property %s is null".format(message.toString, propertyDescriptor.name))
    else
      value
  }

  protected def getValue(message: B): valueHandler.V
}

object Field {
  def apply[B <: AnyRef](tag: Int, prop: PropertyDescriptor) =
      for (vh <- ValueHandler(prop.scalaType))
      yield new Field[B](tag, prop) {
              val valueHandler = vh
              protected def getValue(message: B) = prop.get[valueHandler.V](message)
            }
}



