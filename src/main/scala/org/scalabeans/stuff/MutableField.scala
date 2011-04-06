package org.scalabeans
package stuff

import com.dyuproject.protostuff._
import value._

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



