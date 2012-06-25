package org.scalastuff.scalabeans.format.json

import java.io.InputStream
import java.io.OutputStream
import org.scalastuff.util.StringFormat
import com.fasterxml.jackson.core.JsonFactory
import java.io.ByteArrayOutputStream
import java.io.StringWriter

class JsonFormat[A](jsonFactory: JsonFactory, handler: JsonHandler) extends StringFormat[A] {
  import org.scalastuff.util.Loan._

  def readFrom(str: String): A = {
    val parser = jsonFactory.createJsonParser(str)

    parser.nextToken()
    loan(parser) {
      handler.parse(parser).asInstanceOf[A]
    }
  }

  def readFrom(buffer: Array[Byte]): A = {
    val parser = jsonFactory.createJsonParser(buffer)

    parser.nextToken()
    loan(parser) {
      handler.parse(parser).asInstanceOf[A]
    }
  }

  def readFrom(inputStream: InputStream): A = {
    val parser = jsonFactory.createJsonParser(inputStream)

    parser.nextToken()
    loan(parser) {
      handler.parse(parser).asInstanceOf[A]
    }
  }

  def writeTo(outputStream: OutputStream, obj: A) {
    val generator = jsonFactory.createJsonGenerator(outputStream)
    
    loan(generator) {
      handler.write(generator, obj)
    }
  }
  
  def toByteArray(obj: A): Array[Byte] = {
    val os = new ByteArrayOutputStream()
    writeTo(os, obj)
    os.toByteArray()
  }
  
  def toString(obj: A): String = {
    val str = new StringWriter()
    val generator = jsonFactory.createJsonGenerator(str)
    
    loan(generator) {
      handler.write(generator, obj)
    }
    
    str.toString()
  }
}