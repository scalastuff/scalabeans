package org.scalabeans.stuff
package value

import org.scalabeans.Preamble._
import com.dyuproject.protostuff.{Pipe, Input, Output}
import org.scalabeans.{ScalaType, MutablePropertyDescriptor, ConstructorParameter, BeanDescriptor}

abstract class BeanValueHandler extends ValueHandler {
  type V = AnyRef

  def writeSchema: WriteMirrorSchema[V]

  def readSchema: MirrorSchema[_]

  override def isDefaultValue(v: V) = false

  def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) {
    output.writeObject(tag, value, writeSchema, repeated)
  }

  def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
    output.writeObject(tag, pipe, writeSchema.pipeSchema, repeated)
  }
}

object BeanValueHandler {
  def apply(beanType: ScalaType) = {
    val beanDescriptor = descriptorOf(beanType)
    if (beanDescriptor.hasImmutableConstructorParameters) new ImmutableBeanValueHandler(beanDescriptor)
    else new MutableBeanValueHandler(beanType)
  }
}

class MutableBeanValueHandler(beanType: ScalaType) extends BeanValueHandler {

  override lazy val writeSchema = MirrorSchema.schemaOf[V](beanType)

  def readSchema = writeSchema

  def defaultValue = {
    val value = writeSchema.newMessage()
    writeSchema.resetAllFieldsToDefault(value)
    value
  }

  def readFrom(input: Input) = input.mergeObject(null, writeSchema)
}

class ImmutableBeanValueHandler(beanDescriptor: BeanDescriptor) extends BeanValueHandler {
  def writeSchema = readSchema.writeSchema
  lazy val readSchema = BeanBuilderSchema(beanDescriptor)

  override def defaultValue = {
    val builder = readSchema.newMessage
    for (field <- readSchema.fields)
      field.setDefaultValue(builder)

    builder.result()
  }

  override def readFrom(input: Input) = {
    val builder = input.mergeObject(null, readSchema)
    builder.result()
  }
}