package org.scalabeans
package stuff

import com.dyuproject.protostuff._
import value.ValueHandler

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

object MirrorField {
  def apply[B <: AnyRef](tag: Int, prop: PropertyDescriptor) =
      for (vh <- ValueHandler(prop.scalaType))
      yield new Field[B](tag, prop) {
              val valueHandler = vh
              protected def getValue(message: B) = prop.get[valueHandler.V](message)
            }
}



