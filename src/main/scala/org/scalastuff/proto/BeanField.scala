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

import com.dyuproject.protostuff.Input
import value.{MutableBeanValueHandler, BeanValueHandler}

trait BeanField[B <: AnyRef] extends MutableField[B] {

  override val valueHandler: MutableBeanValueHandler

  override def setDefaultValue(obj: B) {
    var value = getValue(obj)
    if (value == null) {
      val value = valueHandler.defaultValue
      setValue(obj, value)
    } else
      valueHandler.readSchema.resetAllFieldsToDefault(value)
  }

  override def mergeFrom(input: Input, message: B) {
    val existingValue = getValue(message)
    setValue(message, input.mergeObject(existingValue, valueHandler.readSchema))
  }

}