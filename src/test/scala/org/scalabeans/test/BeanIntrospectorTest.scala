package org.scalabeans
package test

import Preamble._
import org.junit.Test
import org.junit.Assert._

class BeanIntrospectorTest {

  @Test
  def testConstructorParameters() {
    val bd = descriptorOf[NotConstructorPropertyBean]

    assertTrue(bd("i").isInstanceOf[ConstructorParameter])
    assertFalse(bd("s").isInstanceOf[ConstructorParameter])
  }

  @Test
  def testCanCreateDescriptorOfAbstractClass() {
    val bd = descriptorOf[AbstractBean]

    assertNotNull(bd)
    assertEquals(1, bd.properties.size)
    assertNotNull(bd("i"))
  }

  @Test
  def testOverrideInConstructorParam() {
    val bd = descriptorOf[OverrideSuperPropertyBean]

    assertNotNull(bd)
    assertEquals(1, bd.properties.size)
    assertNotNull(bd("i"))
    assertTrue(bd("i").isInstanceOf[ConstructorParameter])
    assertEquals(scalaTypeOf[Seq[Int]], bd("i").scalaType)
  }
}

abstract class AbstractBean(val i: Traversable[Int])

class NotConstructorPropertyBean(_s: String, _i: Traversable[Int]) extends AbstractBean(_i) {
  val s = _s + "x"
}

class OverrideSuperPropertyBean(override val i: Seq[Int]) extends AbstractBean(i)