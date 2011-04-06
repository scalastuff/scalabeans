package org.scalabeans.stuff

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