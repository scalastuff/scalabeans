package org.scalastuff.scalabeans

import org.junit.Test
import org.junit.Assert._
import Preamble._
import org.scalastuff.scalabeans.types._

class BeanDescriptorTest {
  import testbeans._

  @Test
  def testFindProperties() {
    val bd = descriptorOf[PropertiesTestBean]

    val immutableCP = bd("immutableCP")
    assertNotNull(immutableCP)
    assertEquals("immutableCP", immutableCP.name)

    val mutableCP = bd.property("mutableCP")
    assertFalse(mutableCP.isEmpty)
    assertEquals("mutableCP", mutableCP.get.name)

    assertTrue(bd.property("whatever").isEmpty)
  }

  @Test
  def testPropertyGet() {
    val bd = descriptorOf[PropertiesTestBean]

    val testBean = new PropertiesTestBean
    testBean.mutableCP = "value"
    val value = bd.get(testBean, "mutableCP")
    assertEquals("value", value)
  }

  @Test
  def testPropertySet() {
    val bd = descriptorOf[PropertiesTestBean]

    val testBean = new PropertiesTestBean
    bd.set(testBean, "mutableCP", "value")
    assertEquals("value", testBean.mutableCP)
  }

  @Test
  def testRename() {
    def testBD(name: String) = descriptorOf[PropertiesTestBean] rewrite propertyRules {
      case p: PropertyDescriptor if p.name == name => p.rename("renamed")
    }

    def checkImmutable(name: String, expected: String) {
      val bd = testBD(name)
      val pd = bd.property(name)
      assertEquals(None, pd)
      assertEquals(5, bd.properties.size)

      val testBean = new PropertiesTestBean
      val readValue = bd.get(testBean, "renamed")
      assertEquals(expected, readValue)
    }

    def checkMutable(name: String, expected: String, writeValue: String) {
      checkImmutable(name, expected)

      val bd = testBD(name)
      val testBean = new PropertiesTestBean
      bd.set(testBean, "renamed", writeValue)
      assertEquals(writeValue, descriptorOf[PropertiesTestBean].get(testBean, name))
    }

    checkImmutable("immutableCP", "1")
    checkMutable("mutableCP", "2", "20")
    checkImmutable("immutable", "3")
    checkMutable("mutable", "4", "40")
  }

  @Test
  def testRemoveProperty() {
    def check(propertyName: String) {
      val bd = descriptorOf[PropertiesTestBean].exclude(propertyName)

      val property1 = bd.property(propertyName)
      assertEquals(None, property1)
      assertEquals(4, bd.properties.size)
    }

    check("immutable")
    check("mutable")
  }

  @Test
  def testPropertyIndex() {
    val bd = descriptorOf[PropertiesTestBean]

    assertEquals(0, bd("immutableCP").asInstanceOf[ConstructorParameter].index)
    assertEquals(1, bd("mutableCP").asInstanceOf[ConstructorParameter].index)
    assertEquals(0, bd("mutable").asInstanceOf[MutablePropertyDescriptor].index)
    assertEquals(1, bd("mutable2").asInstanceOf[MutablePropertyDescriptor].index)

    val bd2 = descriptorOf[PropertiesTestBean].exclude("mutable")

    assertEquals(0, bd2("immutableCP").asInstanceOf[ConstructorParameter].index)
    assertEquals(1, bd2("mutableCP").asInstanceOf[ConstructorParameter].index)
    assertEquals(0, bd2("mutable2").asInstanceOf[MutablePropertyDescriptor].index)
  }

  @Test
  def testRemoveCPProperty() {
    def check(propertyName: String) {
      try {
        descriptorOf[PropertiesTestBean].exclude(propertyName)

        fail("Exception expected when trying to delete constructor parameter " + propertyName)
      } catch {
        case e: Exception => // e.printStackTrace() // success
      }
    }

    check("immutableCP")
    check("mutableCP")
  }

  @Test
  def testConvertPropertyType() {
    def testBD(propertyName: String) = descriptorOf[PropertiesTestBean] rewrite propertyRules {
      case p: PropertyDescriptor if p.name == propertyName => p.convertValue[String, Int](_.toInt, _.toString)
    }

    def checkImmutable(propertyName: String, expected: Int) {
      val bd = testBD(propertyName)
      val pd = bd.property(propertyName)
      assertEquals(false, pd.isEmpty)
      assertEquals(5, bd.properties.size)

      val testBean = new PropertiesTestBean
      val readValue = bd.get(testBean, propertyName)
      assertEquals(expected, readValue)
    }

    def checkMutable(propertyName: String, expected: Int, writeValue: Int) {
      checkImmutable(propertyName, expected)
      val bd = testBD(propertyName)
      val testBean = new PropertiesTestBean
      bd.set(testBean, propertyName, writeValue)
      assertEquals(writeValue.toString, descriptorOf[PropertiesTestBean].get(testBean, propertyName))
    }

    checkImmutable("immutableCP", 1)
    checkMutable("mutableCP", 2, 20)
    checkImmutable("immutable", 3)
    checkMutable("mutable", 4, 40)
  }

  @Test
  def testConvertConstructorParameterType() {
    val bd = descriptorOf[PropertiesTestBean] rewrite propertyRules {
      case p: PropertyDescriptor if p.scalaType == StringType => p.convertValue[String, Int](_.toInt, _.toString)
    }

    val bean = bd.newInstance(10, 20).asInstanceOf[PropertiesTestBean]
    assertEquals("10", bean.immutableCP)
    assertEquals(10, bd.get(bean, "immutableCP"))
    assertEquals("20", bean.mutableCP)
    assertEquals(20, bd.get(bean, "mutableCP"))
  }

  @Test
  def testWithConstructor1() {
    def check(bean: PropertiesTestBean, bd: BeanDescriptor) {
      assertEquals("10", bean.immutableCP)
      assertEquals(10, bd.get(bean, "immutableCP"))
      assertEquals("20", bean.mutableCP)
      assertEquals(20, bd.get(bean, "mutableCP"))
    }
    
    val bdBefore = descriptorOf[PropertiesTestBean].
      withConstructor({ s1: String => new PropertiesTestBean(s1, "20") }, "immutableCP" -> None).
      rewrite(propertyRules {
        case p: PropertyDescriptor if p.scalaType == StringType => p.convertValue[String, Int](_.toInt, _.toString)
      })

    val bdAfter = descriptorOf[PropertiesTestBean] rewrite propertyRules {
      case p: PropertyDescriptor if p.scalaType == StringType => p.convertValue[String, Int](_.toInt, _.toString)
    } withConstructor ({ i1: Int => new PropertiesTestBean(i1.toString(), "20") }, "immutableCP" -> None)

    val bean1 = bdBefore.newInstance(10).asInstanceOf[PropertiesTestBean]
    check(bean1, bdBefore)
    
    val bean2 = bdAfter.newInstance(10).asInstanceOf[PropertiesTestBean]
    check(bean2, bdAfter)
  }

  @Test
  def testDeepRewrite() {
    val bd = descriptorOf[DeepTestBean] rewrite propertyRules {
      case p: PropertyDescriptor if p.name == "mutable" => p.rename("renamed")
    }

    assertEquals(5, bd.properties.size)
    assertNotNull(bd("renamed"))

    //bd.properties foreach {p => println(p.toString)}

    assertHasRenamedProperty(bd("list").scalaType.arguments(0))
    assertHasRenamedProperty(bd("beanArray").scalaType.arguments(0))
    assertHasRenamedProperty(bd("beanMapArray").scalaType.arguments(0).arguments(0).arguments(1))
  }

  @Test
  def testCyclicBean {
    val bd = descriptorOf[CyclicTestBean] rewrite propertyRules {
      case p: PropertyDescriptor if p.name == "cyclicRef" => p.rename("renamed")
    }

    //bd.properties foreach {p => println(p.toString)}

    def checkDeepCycle(beanType: ScalaType, counter: Int) {
      if (counter > 0) {
        assertHasRenamedProperty(beanType)
        val renamed = beanType.asInstanceOf[BeanType].beanDescriptor("renamed")
        checkDeepCycle(renamed.scalaType, counter - 1)
      }
    }
    checkDeepCycle(BeanType(bd), 10)
    checkDeepCycle(bd("renamed").scalaType, 10)
    checkDeepCycle(bd("cyclicSet").scalaType.arguments(0), 10)
    checkDeepCycle(bd("cyclicArray").scalaType.arguments(0), 10)
    checkDeepCycle(bd("cyclicMap").scalaType.arguments(0).arguments(1).arguments(0), 10)
  }

  private def assertHasRenamedProperty(beanType: ScalaType) {
    beanType match {
      case BeanType(bd) =>
        //bd.properties foreach {p => println(p.toString)}
        assertNotNull(bd("renamed"))
      case _ => fail("BeanType expected, %s found".format(beanType))
    }
  }
}

package testbeans {
  class PropertiesTestBean(val immutableCP: String = "1", var mutableCP: String = "2") {
    val immutable: String = "3"
    var mutable: String = "4"
    var mutable2: String = "5"
  }

  class DeepTestBean {
    var mutable: Int = _
    var list: List[PropertiesTestBean] = _
    var arr: Array[Int] = _
    var beanArray: Array[PropertiesTestBean] = _
    var beanMapArray: Array[Map[Int, PropertiesTestBean]] = _
  }

  class CyclicTestBean {
    var cyclicRef: CyclicTestBean = _
    var cyclicSet: Set[CyclicTestBean] = _
    var cyclicArray: Array[CyclicTestBean] = _
    var cyclicMap: Map[String, List[CyclicTestBean]] = _
  }
}