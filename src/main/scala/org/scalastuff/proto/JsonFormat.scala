/*
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

import org.codehaus.jackson.io.IOContext
import org.codehaus.jackson.impl.{Utf8StreamParser, Utf8Generator}
import com.dyuproject.protostuff.{JsonInputException, JsonInput, JsonOutput, JsonIOUtil}
import java.io.{InputStream, OutputStream, ByteArrayOutputStream}
import org.codehaus.jackson.{JsonParser, JsonToken, JsonEncoding}

class JsonFormat(numeric: Boolean) extends SerializationFormat {
  import JsonIOUtil._
  import org.scalastuff.util.Loan._

  def writeTo[B <: AnyRef](writer: BeanWriter[B], bean:B, outputStream: OutputStream) {
    val context = new IOContext(DEFAULT_JSON_FACTORY._getBufferRecycler, outputStream, false)
    context.setEncoding(JsonEncoding.UTF8)
    val generator = new Utf8Generator(context,
      DEFAULT_JSON_FACTORY.getGeneratorFeatures,
      DEFAULT_JSON_FACTORY.getCodec,
      outputStream, context.allocWriteEncodingBuffer, 0, true)

    loan(generator) {
      generator.writeStartObject

      val output = new JsonOutput(generator, numeric, writer.schema)
      writer.writeTo(output, bean)
      if (output.isLastRepeated)
        generator.writeEndArray

      generator.writeEndObject
    }
  }

  def toByteArray[B <: AnyRef](writer: BeanWriter[B], bean:B): Array[Byte] = {
    val outputStream = new ByteArrayOutputStream()
    this.writeTo(writer, bean, outputStream)
    outputStream.toByteArray
  }

  def readFrom[B <: AnyRef](reader: BeanReader[B], inputStream: InputStream) = {
    val context = new IOContext(DEFAULT_JSON_FACTORY._getBufferRecycler(), inputStream, false)
    val parser = new Utf8StreamParser(context,
      DEFAULT_JSON_FACTORY.getParserFeatures,
      inputStream,
      DEFAULT_JSON_FACTORY.getCodec,
      DEFAULT_JSON_FACTORY.getRootByteSymbols.makeChild(true, true),
      context.allocReadIOBuffer(),
      0,
      0,
      true)

    loan(parser) {
      parse(parser, reader)
    }
  }

  def fromByteArray[B <: AnyRef](reader: BeanReader[B], buffer: Array[Byte]): B = {
    val context = new IOContext(DEFAULT_JSON_FACTORY._getBufferRecycler(), buffer, false)
    val parser = new Utf8StreamParser(context,
      DEFAULT_JSON_FACTORY.getParserFeatures,
      null,
      DEFAULT_JSON_FACTORY.getCodec,
      DEFAULT_JSON_FACTORY.getRootByteSymbols.makeChild(true, true),
      buffer,
      0,
      buffer.length,
      false)

    loan(parser) {
      parse(parser, reader)
    }
  }

  def parse[B <: AnyRef](parser: JsonParser, reader: BeanReader[B]): B = {
    if (parser.nextToken != JsonToken.START_OBJECT) {
      throw new JsonInputException("Expected token: { but was " + parser.getCurrentToken)
    }
    val input = new JsonInput(parser, numeric)
    val bean = reader.readFrom(input)

    if (parser.getCurrentToken != JsonToken.END_OBJECT) {
      throw new JsonInputException("Expected token: } but was " + parser.getCurrentToken)
    }

    bean
  }
}

object JsonFormat extends JsonFormat(false)

/**
 * Uses property tag as field name
 */
object JsonFieldTagsFormat extends JsonFormat(true)