package org.scalabeans.stuff

import org.junit.{Test, Assert}
import com.dyuproject.protostuff.{LinkedBuffer, GraphIOUtil, ProtobufIOUtil}

class NestedInlinedTypesTest {
  val linkedBuffer = LinkedBuffer.allocate(512)

  @Test
  def testSimple() {
    val schema = MirrorSchema.schemaOf[NestedInlinedTestBean]

    def checkSerDeser(nitb: NestedInlinedTestBean) = {
      linkedBuffer.clear()
      val buffer:Array[Byte] = ProtobufIOUtil.toByteArray(nitb, schema, linkedBuffer)
      println("NestedInlinedTestBean: " + (buffer mkString " "))
      val deser1 = new NestedInlinedTestBean()
      ProtobufIOUtil.mergeFrom(buffer, deser1, schema)
      nitb.assertEquals(deser1)
    }

    checkSerDeser(new NestedInlinedTestBean())
    checkSerDeser(new NestedInlinedTestBean().set1())
  }
}


class NestedInlinedTestBean {
  var lo = List[Option[String]]()
  var ol: Option[List[String]] = None
  var l3 = List[List[List[String]]]()

  def set1() = {
    lo = List(Some("str"), None, Some("other"))
    ol = Some(List("e1", "e2"))
    l3 = List(List(List("0-0-1", "0-0-2"), List("0-1-1", "0-1-2")))

    this
  }

  def assertEquals(other: NestedInlinedTestBean) {
    Assert.assertEquals(lo, other.lo)
    Assert.assertEquals(ol, other.ol)
    Assert.assertEquals(l3, other.l3)
  }
}