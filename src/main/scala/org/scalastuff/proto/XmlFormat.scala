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

import com.dyuproject.protostuff.XmlIOFactoryUtil._
import javax.xml.stream.XMLStreamConstants._
import com.dyuproject.protostuff.{XmlInput, XmlInputException, XmlOutput, XmlIOUtil}
import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}

object XmlFormat extends SerializationFormat {
  import XmlIOUtil._
  import org.scalastuff.util.Loan._

  def writeTo[B <: AnyRef](beanWriter: BeanWriter[B], bean:B, outputStream: OutputStream) {
    val xmlWriter = DEFAULT_OUTPUT_FACTORY.createXMLStreamWriter(outputStream, XML_ENCODING)
    loan(xmlWriter) {
      xmlWriter.writeStartDocument(XML_ENCODING, XML_VERSION)
      xmlWriter.writeStartElement(beanWriter.schema.messageName)

      val output = new XmlOutput(xmlWriter, beanWriter.schema)
      beanWriter.writeTo(output, bean)

      xmlWriter.writeEndElement
      xmlWriter.writeEndDocument
      xmlWriter.flush
    }
  }

  def toByteArray[B <: AnyRef](beanWriter: BeanWriter[B], bean:B): Array[Byte] = {
    val outputStream = new ByteArrayOutputStream()
    this.writeTo(beanWriter, bean, outputStream)
    outputStream.toByteArray
  }

  def readFrom[B <: AnyRef](reader: BeanReader[B], inputStream: InputStream) = {
    val parser = DEFAULT_INPUT_FACTORY.createXMLStreamReader(inputStream, XML_ENCODING)
    loan(parser) {
      if (parser.nextTag() != START_ELEMENT || !reader.messageName.equals(parser.getLocalName)) {
        throw new XmlInputException("Expected token START_ELEMENT: " + reader.messageName )
      }

      parser.nextTag()

      val input = new XmlInput(parser)
      reader.readFrom(input)
    }
  }

  def fromByteArray[B <: AnyRef](reader: BeanReader[B], buffer: Array[Byte]): B = {
    val inputStream = new ByteArrayInputStream(buffer)
    this.readFrom(reader, inputStream)
  }
}