package org.scalabeans.stuff.value

import org.junit.{Test, Assert}
import org.scalabeans.Preamble._
import org.scalabeans._

class BeanValueHandlerTest {

  @Test
  def testImmutable {
    val bd = descriptorOf[Tuple2[String, Int]]
    val vh = BeanValueHandler(scalaTypeOf[Tuple2[String, Int]])

    Assert.assertEquals(2, bd.properties.size)
    val p1 = bd.properties(0)
    Assert.assertTrue(p1.isInstanceOf[ImmutablePropertyDescriptor])
    Assert.assertTrue(p1.isInstanceOf[ConstructorParameter])
    Assert.assertEquals(StringType, p1.scalaType)

    Assert.assertTrue(bd.hasImmutableConstructorParameters)
    Assert.assertTrue(vh.isInstanceOf[ImmutableBeanValueHandler])

    val ivh = new ImmutableBeanValueHandler(bd)
    Assert.assertEquals(2, ivh.readSchema.fields.size)

    val builder = ivh.readSchema.newMessage
    Assert.assertEquals(2, builder.constructorParams.size)
  }
}