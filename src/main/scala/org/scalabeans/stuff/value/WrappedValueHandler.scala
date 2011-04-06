package org.scalabeans.stuff.value

import com.dyuproject.protostuff.{Pipe, Output, Input}
import org.scalabeans.stuff.{BeanBuilderSchema, MirrorSchema}
import org.scalabeans.Preamble._
import org.scalabeans.{ScalaType, TupleType}

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