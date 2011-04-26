package org.scalastuff.proto

import com.dyuproject.protostuff.{ProtobufOutput, LinkedBuffer, ByteArrayInput}
import org.junit.{Test, Assert}
import Preamble._

class BeanReaderWriterTest {
  val linkedBuffer = LinkedBuffer.allocate(512)

  @Test
  def testVarConstructorParameters {
    val writer = writerOf[VarConstructorParameterBean]
    val reader = readerOf[VarConstructorParameterBean]

    def checkSerDeser(ptb: VarConstructorParameterBean) {
      linkedBuffer.clear()
      
      val output = new ProtobufOutput(linkedBuffer)
      writer.writeTo(output, ptb)
      val buffer:Array[Byte] = output.toByteArray()
      println("VarConstructorParameterBean: " + (buffer mkString " "))
      
      val input = new ByteArrayInput(buffer, false)
      val deser1 = reader.readFrom(input)
      ptb.assertEquals(deser1)
    }

    checkSerDeser(new VarConstructorParameterBean("a"))
    checkSerDeser(new VarConstructorParameterBean("b").set1())
  }
}

class VarConstructorParameterBean(var s: String) {
  def set1() = {
    s = "whatever"
    this
  }
  
  def assertEquals(other: VarConstructorParameterBean) {
    Assert.assertEquals(s, other.s)
  }
}