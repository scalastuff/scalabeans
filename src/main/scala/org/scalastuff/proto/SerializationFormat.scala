/**
 * Copyright (c) 2011 ScalaStuff.org (joint venture of Alexander Dvorkovyy and Ruud Diterwich)
 * Copyright 2007-2011 David Yu dyuproject@gmail.com (derived from work)
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

import com.dyuproject.protostuff._
import java.io.{OutputStream, InputStream}

abstract class SerializationFormat {
  def writeTo[B <: AnyRef](writer: BeanWriter[B], bean:B, output: OutputStream): Unit
  def toByteArray[B <: AnyRef](writer: BeanWriter[B], bean:B): Array[Byte]
  def readFrom[B <: AnyRef](reader: BeanReader[B], input: InputStream): B
  def fromByteArray[B <: AnyRef](reader: BeanReader[B], buffer: Array[Byte]): B
}

object SerializationFormat {
  val linkedBufferPool = new ThreadLocal[LinkedBuffer] {
    override protected def initialValue() = LinkedBuffer.allocate(512)
  }

  def getLinkedBuffer() = {
    val linkedBuffer = linkedBufferPool.get
    linkedBuffer.clear()
    linkedBuffer
  }
}

object ProtobufFormat extends SerializationFormat {
  def writeTo[B <: AnyRef](writer: BeanWriter[B], bean:B, outputStream: OutputStream) {
    val linkedBuffer = SerializationFormat.getLinkedBuffer()
    val output = new ProtobufOutput(linkedBuffer)
    writer.writeTo(output, bean)
    LinkedBuffer.writeTo(outputStream, linkedBuffer)
  }

  def toByteArray[B <: AnyRef](writer: BeanWriter[B], bean:B): Array[Byte] = {
    val linkedBuffer = SerializationFormat.getLinkedBuffer()
    val output = new ProtobufOutput(linkedBuffer)
    writer.writeTo(output, bean)
    output.toByteArray()
  }

  def readFrom[B <: AnyRef](reader: BeanReader[B], inputStream: InputStream) = {
    val input = new CodedInput(inputStream, false)
    reader.readFrom(input)
  }

  def fromByteArray[B <: AnyRef](reader: BeanReader[B], buffer: Array[Byte]): B = {
    val input = new ByteArrayInput(buffer, false)
    reader.readFrom(input)
  }
}

object GraphProtostuffFormat extends SerializationFormat {
  def writeTo[B <: AnyRef](writer: BeanWriter[B], bean:B, outputStream: OutputStream) {
    val linkedBuffer = SerializationFormat.getLinkedBuffer()
    val output = new ProtostuffOutput(linkedBuffer, outputStream)
    val graphOutput = new GraphProtostuffOutput(output)
    writer.writeTo(graphOutput, bean)
    LinkedBuffer.writeTo(outputStream, linkedBuffer)
  }

  def toByteArray[B <: AnyRef](writer: BeanWriter[B], bean:B): Array[Byte] = {
    val linkedBuffer = SerializationFormat.getLinkedBuffer()
    val output = new ProtostuffOutput(linkedBuffer)
    val graphOutput = new GraphProtostuffOutput(output)
    writer.writeTo(graphOutput, bean)
    output.toByteArray()
  }

  def readFrom[B <: AnyRef](reader: BeanReader[B], inputStream: InputStream) = {
    val input = new CodedInput(inputStream, true)
    val graphInput = new GraphCodedInput(input)
    reader.readFrom(graphInput)
  }

  def fromByteArray[B <: AnyRef](reader: BeanReader[B], buffer: Array[Byte]): B = {
    val input = new ByteArrayInput(buffer, true)
    val graphInput = new GraphByteArrayInput(input)
    reader.readFrom(graphInput)
  }
}