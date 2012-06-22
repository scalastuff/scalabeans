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

import value.{ValueHandler, RepeatedValueHandler, MutableBeanValueHandler}
import org.scalastuff.scalabeans._

class BeanBuilderSchema(_beanDescriptor: BeanDescriptor, override val fields: Seq[PropertyBuilderField])
  extends BeanSchema[BeanBuilder](_beanDescriptor, fields) {

  override def newMessage = beanDescriptor.newBuilder

  val writeSchema = {
    val writeFields = fields map (_.readOnlyField)

    new WriteBeanSchema[AnyRef](beanDescriptor, writeFields)
  }
}

object BeanBuilderSchema {
  def apply(beanDescriptor: BeanDescriptor) = {
    val properties = beanDescriptor.properties collect {
      case cp: ConstructorParameter => cp
      case mp: MutablePropertyDescriptor => mp
    }

    val fields =
      for {
        property <- properties
        field <- PropertyBuilderField(property.tag, property)
      } yield field

    new BeanBuilderSchema(beanDescriptor, fields)
  }
}

abstract class PropertyBuilderField(tag: Int, propertyDescriptor: PropertyDescriptor)
  extends MutableField[BeanBuilder](tag, propertyDescriptor) {

  def readOnlyField = Field[AnyRef](tag, propertyDescriptor).get // do we ever get None here?

  def getValue(message: BeanBuilder) = message.get(propertyDescriptor).asInstanceOf[valueHandler.V]

  def setValue(message: BeanBuilder, value: valueHandler.V) {
    message.set(propertyDescriptor, value)
  }
}

object PropertyBuilderField {
  def apply(tag: Int, prop: PropertyDescriptor) =
      for (valueHandler <- ValueHandler(prop.metamodel))
      yield {
        valueHandler match {
          case repeatedValueHandler: RepeatedValueHandler =>
            new PropertyBuilderField(tag, prop) with RepeatedField[BeanBuilder] {
              val valueHandler = repeatedValueHandler
            }
          case beanValueHandler: MutableBeanValueHandler =>
            new PropertyBuilderField(tag, prop) with BeanField[BeanBuilder] {
              val valueHandler = beanValueHandler
            }
          case vh @ _ =>
            new PropertyBuilderField(tag, prop) {
              val valueHandler = vh
            }
        }
      }
}
