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

import org.scalastuff.scalabeans.types.ScalaType
import com.dyuproject.protostuff.{Pipe, Schema, Output, Input}
import collection.mutable.{ArrayBuffer, Buffer, HashMap}
import org.scalastuff.scalabeans._
import org.scalastuff.scalabeans.Preamble._

class WriteMirrorSchema[T <: AnyRef](val beanDescriptor: BeanDescriptor, val fields: Seq[Field[T]]) extends Schema[T] {

  def mergeFrom(input: Input, message: T) {
    throw new UnsupportedOperationException
  }

  def newMessage: T = throw new UnsupportedOperationException

  def isInitialized(message: T) = true

  def writeTo(output: Output, message: T) {
    for (field <- fields)
      field.writeTo(output, message)
  }

  def getFieldNumber(name: String) = tagsByName.get(name) getOrElse 0

  def getFieldName(number: Int) = fieldsByNumber(number) map (_.name) getOrElse "unknown"

  def typeClass: Class[T] = beanDescriptor.beanType.erasure.asInstanceOf[Class[T]]

  def messageFullName = beanDescriptor.beanType.erasure.getName

  def messageName = beanDescriptor.beanType.erasure.getSimpleName

  val pipeSchema: Schema[Pipe] = new Pipe.Schema[T](this) {
    protected def transfer(pipe: Pipe, input: Input, output: Output) {
      var tag = input.readFieldNumber(this)
      while (tag != 0) {
        val field =
          if (tag < fieldsByNumber.length) fieldsByNumber(tag) orNull
          else null

        if (field == null)
          input.handleUnknownField(tag, wrappedSchema);
        else
          field.transfer(pipe, input, output, field.repeated);

        tag = input.readFieldNumber(this)
      }
    }
  }

  protected val tagsByName: Map[String, Int] = Map(fields map {
    field => (field.name, field.tag)
  }: _*)

  protected val fieldsByNumber: Seq[Option[Field[T]]] = mapFieldsByNumber(fields)

  protected def mapFieldsByNumber[F <: Field[T]](fields: Seq[F]): Seq[Option[F]] = {
    val lastPropertyTag: Int = fields.foldLeft(0) {
      (maxTag, field) => math.max(maxTag, field.tag)
    }

    val fieldsByNumber = Buffer.fill[Option[F]](lastPropertyTag + 1)(None)

    for (field <- fields)
      fieldsByNumber(field.tag) = Some(field)

    fieldsByNumber.toSeq
  }

  protected val setFields = {
    val lastFieldTag = fields.foldLeft(0)(_ max _.tag)
    ArrayBuffer.fill[Boolean](lastFieldTag + 1)(false)
  }

}

class MirrorSchema[T <: AnyRef](_beanDescriptor: BeanDescriptor, override val fields: Seq[MutableField[T]])
  extends WriteMirrorSchema[T](_beanDescriptor, fields) {

  override def newMessage = beanDescriptor.newInstance().asInstanceOf[T]

  override def mergeFrom(input: Input, message: T) {
    val fieldIsSet = this.setFields.clone()

    val builders = HashMap[Int, RepeatedField[T]#Builder]()
    var lastBuilderTag = -1
    var lastBuilder: RepeatedField[T]#Builder = null

    var tag = input.readFieldNumber(this)
    while (tag != 0) {
      val field =
        if (tag < fieldsByNumber.length) fieldsByNumber(tag) orNull
        else null

      if (field != null) {
        field match {
          case repeated: RepeatedField[T] =>
            if (lastBuilderTag != tag) {
              lastBuilder = builders.getOrElseUpdate(tag, repeated.newBuilder())
              lastBuilderTag = tag
            }

            lastBuilder.mergeElementFrom(input)

          case _ =>
            field.mergeFrom(input, message)
            fieldIsSet(field.tag) = true
        }
      }
      else
        input.handleUnknownField(tag, this)

      tag = input.readFieldNumber(this)
    }

    for ((tag, builder) <- builders) {
      builder.sinkTo(message)
      fieldIsSet(builder.tag) = true
    }

    for {
      field <- fields if !fieldIsSet(field.tag)
    } field.setDefaultValue(message)
  }

  def resetAllFieldsToDefault(message: T) = fields foreach (_.setDefaultValue(message))

  override protected val fieldsByNumber: Seq[Option[MutableField[T]]] = mapFieldsByNumber(fields)
}

object MirrorSchema {
  def schemaOf[T <: AnyRef](implicit mf: Manifest[T]): MirrorSchema[T] = schemaOf[T](scalaTypeOf(mf))

  def schemaOf[T <: AnyRef](beanType: ScalaType) = {
    val beanDescriptor = descriptorOf(beanType)
    
    if (beanDescriptor.needsBeanBuilder)
      throw new IllegalArgumentException("Cannot create MirroSchema for %s: it needs BeanBuilder to be instantiatied".format(beanDescriptor.name))
      
    val fields: Seq[MutableMirrorField[T]] = {
      for {
        prop <- beanDescriptor.properties
        field <- MutableMirrorField[T](prop.tag, prop)
      }
      yield field
    }
    new MirrorSchema[T](beanDescriptor, fields)
  }
}