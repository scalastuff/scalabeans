package org.scalabeans.stuff

import com.dyuproject.protostuff.{ProtobufIOUtil, LinkedBuffer}
import org.junit.{Test, Assert}

class ImmutableTest {
  val linkedBuffer = LinkedBuffer.allocate(512)

  @Test
  def testSimple {
    val schema = MirrorSchema.schemaOf[ImmutableTestBean]

    def checkSerDeser(ptb: ImmutableTestBean) {
      linkedBuffer.clear()
      val buffer:Array[Byte] = ProtobufIOUtil.toByteArray(ptb, schema, linkedBuffer)
      println("ImmutableTestBean: " + (buffer mkString " "))
      val deser1 = new ImmutableTestBean()
      ProtobufIOUtil.mergeFrom(buffer, deser1, schema)
      ptb.assertEquals(deser1)
    }

    checkSerDeser(new ImmutableTestBean())
    checkSerDeser(new ImmutableTestBean().set1())
  }
}

case class Immutable(s: String, i: Int)
class ImmutableTestBean {
  var immutableBean = Immutable("", 0)
  var tuple = (0, "")

  def set1() = {
    immutableBean = Immutable("whatever", 10)
    tuple = (20, "tuple")
    this
  }

  def assertEquals(other: ImmutableTestBean) {
    Assert.assertEquals(immutableBean, other.immutableBean)
    Assert.assertEquals(tuple, other.tuple)
  }
}