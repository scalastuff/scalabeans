package org.scalastuff.util

import java.io.InputStream
import java.io.OutputStream

trait Format[A] {
  def readFrom(buffer: Array[Byte]): A
  def readFrom(inputStream: InputStream): A
  def writeTo(outputStream: OutputStream, obj: A): Unit
  def toByteArray(obj: A): Array[Byte]
}

trait StringFormat[A] extends Format[A] {
  def readFrom(str: String): A
  def toString(obj: A): String
}