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