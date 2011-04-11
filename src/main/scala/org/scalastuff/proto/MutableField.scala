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

import value._
import org.scalastuff.scalabeans._
import com.dyuproject.protostuff._

abstract class MutableField[B <: AnyRef](_tag: Int, _propertyDescriptor: PropertyDescriptor)
  extends Field[B](_tag, _propertyDescriptor) {

  def setDefaultValue(message: B) {
    setValue(message, valueHandler.defaultValue)
  }

  def mergeFrom(input: Input, message: B) {
    setValue(message, valueHandler.readFrom(input))
  }

  protected def setValue(message: B, value: valueHandler.V)
}

abstract class MutableMirrorField[B <: AnyRef](tag: Int, propertyDescriptor: MutablePropertyDescriptor)
  extends MutableField[B](tag, propertyDescriptor) {

  def getValue(message: B) = propertyDescriptor.get[valueHandler.V](message)

  def setValue(message: B, value: valueHandler.V) {
    propertyDescriptor.set(message, value)
  }
}

object MutableMirrorField {
  def apply[B <: AnyRef](tag: Int, prop: PropertyDescriptor) = prop match {
    case mutableProperty: MutablePropertyDescriptor =>
      for (valueHandler <- ValueHandler(mutableProperty.scalaType))
      yield {
        valueHandler match {
          case repeatedValueHandler: RepeatedValueHandler =>
            new MutableMirrorField[B](tag, mutableProperty) with RepeatedField[B] {
              val valueHandler = repeatedValueHandler
            }
          case beanValueHandler: MutableBeanValueHandler =>
            new MutableMirrorField[B](tag, mutableProperty) with BeanField[B] {
              val valueHandler = beanValueHandler
            }
          case vh @ _ =>
            new MutableMirrorField[B](tag, mutableProperty) {
              val valueHandler = vh
            }
        }
      }

    case _ => None
  }
}



