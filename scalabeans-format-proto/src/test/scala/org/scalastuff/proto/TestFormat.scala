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

import java.nio.charset.Charset
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.scalastuff.scalabeans.format.proto.ProtobufFormatFactory
import org.scalastuff.scalabeans._
import org.scalastuff.util.StringFormat
import org.scalastuff.scalabeans.format.proto.GraphProtostuffFormatFactory

trait TestBean[B <: TestBean[B]] {
  def set1(): B
  def assertEquals(other: B): Unit
}

object TestFormat {

  val formats = List(ProtobufFormatFactory(), GraphProtostuffFormatFactory())

  def checkFormats[T <: TestBean[T]](ctor: () => T)(implicit mf: Manifest[T]) {
    for (format <- formats)
      checkTestBeanSerDeser(ctor, format)
  }

  def checkTestBeanSerDeser[T <: TestBean[T]](ctor: () => T, formatFactory: FormatFactory)(implicit mf: Manifest[T]) {
    checkSerDeser(ctor(), formatFactory) { _.assertEquals(_) }
    checkSerDeser(ctor().set1(), formatFactory) { _.assertEquals(_) }
  }

  def checkSerDeserFormats[T <: AnyRef](bean: T)(check: (T, T) => Unit)(implicit mf: Manifest[T]) {
    for (format <- formats)
      checkSerDeser(bean, format)(check)
  }

  def checkSerDeser[T <: AnyRef](bean: T, formatFactory: FormatFactory)(check: (T, T) => Unit)(implicit mf: Manifest[T]) {
    val format = formatFactory.formatFor[T]

    val buffer = format.toByteArray(bean)

    val bufferStr = format match {
      case _: StringFormat[_] => new String(buffer, Charset.forName("UTF-8"))
      case _ => buffer map ("%02X" format _) mkString " "
    }

    println("%s (%s), %d bytes: %s".format(
      mf.erasure.getSimpleName,
      format.getClass.getSimpleName stripSuffix "Format$",
      buffer.length,
      bufferStr))

    val deserialized = format.readFrom(buffer)

    check(bean, deserialized)

    // check ser/deser via streams
    val out = new ByteArrayOutputStream()
    format.writeTo(out, bean)
    val buffer2 = out.toByteArray()

    val in = new ByteArrayInputStream(buffer2)
    val deserializedFromStream = format.readFrom(in)
    check(bean, deserializedFromStream)
  }
}