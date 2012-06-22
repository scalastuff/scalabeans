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
package value

import com.dyuproject.protostuff.{ Pipe, Input, Output, Schema }
import org.scalastuff.scalabeans._
import org.scalastuff.scalabeans.types._
import org.scalastuff.scalabeans.Preamble._
import com.google.common.collect.MapMaker

abstract class BeanValueHandler extends ValueHandler {
  type V = AnyRef

  def writeSchema: Schema[V]

  protected def readSchema: Schema[_]

  override def isDefaultValue(v: V) = false

  def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) {
    output.writeObject(tag, value, writeSchema, repeated)
  }

  def beanWriteTo(output: Output, value: V) {
    writeSchema.writeTo(output, value)
  }

  def beanReadFrom(input: Input): V
}

object BeanValueHandler {
  def apply(beanDescriptor: BeanDescriptor): BeanValueHandler = {   
      if (beanDescriptor.needsBeanBuilder) new ImmutableBeanValueHandler(beanDescriptor)
      else new MutableBeanValueHandler(beanDescriptor)
  }
}

class SchemaValueHandler(schema: Schema[_]) extends BeanValueHandler {
  def writeSchema = schema.asInstanceOf[Schema[V]]
  def readSchema = writeSchema

  def defaultValue = writeSchema.newMessage

  def readFrom(input: Input) = input.mergeObject(null, readSchema)
  def beanReadFrom(input: Input) = {
    val result = readSchema.newMessage()
    readSchema.mergeFrom(input, result)
    result
  }

  def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
    throw new UnsupportedOperationException
  }
}

trait MirrorSchemaValueHandler extends BeanValueHandler {
  override def writeSchema: WriteBeanSchema[V]

  def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
    output.writeObject(tag, pipe, writeSchema.pipeSchema, repeated)
  }
}

class MutableBeanValueHandler(beanDescriptor: BeanDescriptor) extends BeanValueHandler with MirrorSchemaValueHandler {

  override lazy val writeSchema = BeanSchema.schemaOf[V](beanDescriptor)

  def readSchema = writeSchema

  def defaultValue = {
    val value = writeSchema.newMessage()
    writeSchema.resetAllFieldsToDefault(value)
    value
  }

  def readFrom(input: Input) = input.mergeObject(null, readSchema)
  def beanReadFrom(input: Input) = {
    val result = readSchema.newMessage()
    readSchema.mergeFrom(input, result)
    result
  }
}

class ImmutableBeanValueHandler(beanDescriptor: BeanDescriptor) extends BeanValueHandler with MirrorSchemaValueHandler {
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

  def beanReadFrom(input: Input) = {
    val builder = readSchema.newMessage
    readSchema.mergeFrom(input, builder)
    builder.result()
  }
}