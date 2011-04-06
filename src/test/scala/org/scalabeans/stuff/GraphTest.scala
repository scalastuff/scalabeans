package org.scalabeans.stuff

import com.dyuproject.protostuff.{LinkedBuffer, GraphIOUtil}
import org.junit.{Assert, Test}

class GraphTest {
  val linkedBuffer = LinkedBuffer.allocate(512)

  @Test
  def testSimpleCase() {
    val wrapper = new Wrapper
    wrapper.one = new One
    wrapper.two = new Two
    wrapper.one.two = wrapper.two
    wrapper.two.one = wrapper.one

    val schema = MirrorSchema.schemaOf[Wrapper]

    linkedBuffer.clear()
    val buffer = GraphIOUtil.toByteArray(wrapper, schema, linkedBuffer)
    val deser = new Wrapper
    GraphIOUtil.mergeFrom(buffer, deser, schema)

    Assert.assertSame(deser.two, deser.one.two)
    Assert.assertSame(deser.one, deser.two.one)
  }
}

class Wrapper {
  var one: One = _
  var two: Two = _
}

class One {
  var two: Two = _
}

class Two {
  var one: One = _
}