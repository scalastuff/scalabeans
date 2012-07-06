package org.scalastuff.scalabeans

import org.junit.Test
import org.junit.Assert._
import Preamble._
import org.scalastuff.scalabeans.types._
import org.scalastuff.scalabeans.converters.Converter

class BeanDescriptorTest {
  import testbeans._

  @Test
  def testFindProperties() {
    val bd = descriptorOf[PropertiesTestBean]

    val immutableCP = bd.property("immutableCP")
    assertNotNull(immutableCP)
    assertEquals("immutableCP", immutableCP.name)

    val mutableCP = bd.propertyOption("mutableCP")
    assertFalse(mutableCP.isEmpty)
    assertEquals("mutableCP", mutableCP.get.name)

    assertTrue(bd.propertyOption("whatever").isEmpty)
  }

  @Test
  def testPropertyGet() {
    val bd = descriptorOf[PropertiesTestBean]

    val testBean = new PropertiesTestBean
    testBean.mutableCP = "value"
    val value = bd.property("mutableCP").get[Any](testBean)
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
    def testBD(name: String) = (descriptorOf[PropertiesTestBean] rewrite propertyRules {
      case p: PropertyDescriptor if p.name == name => p.withName("renamed")
    }).asInstanceOf[BeanDescriptor]

    def checkImmutable(name: String, expected: String) {
      val bd = testBD(name)
      val pd = bd.propertyOption(name)
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
      val bd = descriptorOf[PropertiesTestBean].withoutProperty(propertyName)

      val property1 = bd.propertyOption(propertyName)
      assertEquals(None, property1)
      assertEquals(4, bd.properties.size)
    }

    check("immutable")
    check("mutable")
  }

  @Test
  def testPropertyIndex() {
    val bd = descriptorOf[PropertiesTestBean]

    assertEquals(0, bd.property("immutableCP").asInstanceOf[ConstructorParameter].index)
    assertEquals(1, bd.property("mutableCP").asInstanceOf[ConstructorParameter].index)
    assertEquals(0, bd.property("mutable").asInstanceOf[MutablePropertyDescriptor].index)
    assertEquals(1, bd.property("mutable2").asInstanceOf[MutablePropertyDescriptor].index)

    val bd2 = descriptorOf[PropertiesTestBean].withoutProperty("mutable")

    assertEquals(0, bd2.property("immutableCP").asInstanceOf[ConstructorParameter].index)
    assertEquals(1, bd2.property("mutableCP").asInstanceOf[ConstructorParameter].index)
    assertEquals(0, bd2.property("mutable2").asInstanceOf[MutablePropertyDescriptor].index)
  }

  @Test
  def testRemoveCPProperty() {
    def check(propertyName: String) {
      try {
        descriptorOf[PropertiesTestBean].withoutProperty(propertyName)

        fail("Exception expected when trying to delete constructor parameter " + propertyName)
      } catch {
        case e: Exception => // e.printStackTrace() // success
      }
    }

    check("immutableCP")
    check("mutableCP")
  }
  
  @Test
  def testRemoveNotExistingProperty() {
    descriptorOf[PropertiesTestBean].withoutProperty("fjhjfdhjgkfdh")
    // no exception expected
  }

  @Test
  def testConvertPropertyType() {
    def testBD(propertyName: String) = descriptorOf[PropertiesTestBean].mapProperties {
      case p @ PropertyDescriptor(propertyName, StringType) => 
        p.withTypeMetaModel(p.typeMetaModel.addConverter(Converter[String, Int](_.toInt, _.toString)))
    }

    def checkImmutable(propertyName: String, expected: Int) {
      val bd = testBD(propertyName)
      val pd = bd.propertyOption(propertyName)
      assertEquals(false, pd.isEmpty)
      assertEquals(5, bd.properties.size)

      val testBean = new PropertiesTestBean
      val readValue = bd.getVisible(testBean, propertyName)
      assertEquals(expected, readValue)
    }

    def checkMutable(propertyName: String, expected: Int, writeValue: Int) {
      checkImmutable(propertyName, expected)
      val bd = testBD(propertyName)
      val testBean = new PropertiesTestBean
      bd.setVisible(testBean, propertyName, writeValue)
      assertEquals(writeValue.toString, descriptorOf[PropertiesTestBean].get(testBean, propertyName))
    }

    checkImmutable("immutableCP", 1)
    checkMutable("mutableCP", 2, 20)
    checkImmutable("immutable", 3)
    checkMutable("mutable", 4, 40)
  }

  @Test
  def testConvertConstructorParameterType() {
    val bd = (descriptorOf[PropertiesTestBean] rewrite metamodelRules {
      case mm @ MetaModel(StringType) => mm.addConverter(Converter[String, Int](_.toInt, _.toString))
    }).asInstanceOf[BeanDescriptor]

    val bean = bd.newInstance(10, 20).asInstanceOf[PropertiesTestBean]
    assertEquals("10", bean.immutableCP)
    assertEquals(10, bd.getVisible(bean, "immutableCP"))
    assertEquals("20", bean.mutableCP)
    assertEquals(20, bd.getVisible(bean, "mutableCP"))
  }

  @Test
  def testWithConstructor1() {
    def check(bean: PropertiesTestBean, bd: BeanDescriptor) {
      assertEquals("10", bean.immutableCP)
      assertEquals(10, bd.getVisible(bean, "immutableCP"))
      assertEquals("20", bean.mutableCP)
      assertEquals(20, bd.getVisible(bean, "mutableCP"))
    }

    val bdBefore = descriptorOf[PropertiesTestBean].
      withConstructor({ s1: String => new PropertiesTestBean(s1, "20") }, "immutableCP" -> None).
      rewrite(metamodelRules {
        case mm @ MetaModel(StringType) => mm.addConverter(Converter[String, Int](_.toInt, _.toString))
      }).asInstanceOf[BeanDescriptor]

    val bdAfter = (descriptorOf[PropertiesTestBean] rewrite metamodelRules {
      case mm @ MetaModel(StringType) => mm.addConverter(Converter[String, Int](_.toInt, _.toString))
    }).asInstanceOf[BeanDescriptor].
    withConstructor ({ s1: String => new PropertiesTestBean(s1, "20") }, "immutableCP" -> None)

    val bean1 = bdBefore.newInstance(10).asInstanceOf[PropertiesTestBean]
    check(bean1, bdBefore)

    val bean2 = bdAfter.newInstance(10).asInstanceOf[PropertiesTestBean]
    check(bean2, bdAfter)
  }

  @Test
  def testDeepRewriteProperty() {
    val bd = (descriptorOf[DeepTestBean] rewrite propertyRules {
      case p @ PropertyDescriptor("mutable", _) => p.withName("renamed")
    }).asInstanceOf[BeanDescriptor]

    assertEquals(5, bd.properties.size)
    assertFalse(bd.propertyOption("renamed").isEmpty)

    //bd.properties foreach {p => println(p.toString)}

    val bd1 = bd.property("list").typeMetaModel.asInstanceOf[ContainerMetaModel].elementMetaModel
    val bd2 = bd.property("beanArray").typeMetaModel.asInstanceOf[ContainerMetaModel].elementMetaModel
    //assertEquals(bd1, bd2)
    
    val bd3 = bd.property("beanMapArray").typeMetaModel.asInstanceOf[ContainerMetaModel].
    	elementMetaModel.asInstanceOf[ContainerMetaModel].
    	elementMetaModel.asInstanceOf[BeanDescriptor].property("_2").typeMetaModel
    
    assertHasProperty(bd1, "renamed")
    assertHasProperty(bd2, "renamed")
    assertHasProperty(bd3, "renamed")

    val st1 = bd.property("list").visibleType.arguments(0)
    val st2 = bd.property("beanArray").visibleType.arguments(0)
    assertEquals(st1, st2)

  }

  @Test
  def testDeepRewriteBean() {
    val bd = (descriptorOf[DeepTestBean] rewrite metamodelRules {
      case bd: BeanDescriptor => bd.withoutProperty("mutable")
    }).asInstanceOf[BeanDescriptor]

    assertEquals(4, bd.properties.size)
    assertEquals(None, bd.propertyOption("mutable"))
    
    val bd1 = bd.property("list").typeMetaModel.asInstanceOf[ContainerMetaModel].elementMetaModel
    val bd2 = bd.property("beanArray").typeMetaModel.asInstanceOf[ContainerMetaModel].elementMetaModel
    //assertEquals(bd1, bd2)
    
    val bd3 = bd.property("beanMapArray").typeMetaModel.asInstanceOf[ContainerMetaModel].
    	elementMetaModel.asInstanceOf[ContainerMetaModel].
    	elementMetaModel.asInstanceOf[BeanDescriptor].property("_2").typeMetaModel

    assertHasNoProperty(bd1, "mutable")
    assertHasNoProperty(bd2, "mutable")
    assertHasNoProperty(bd3, "mutable")

    val st1 = bd.property("list").visibleType.arguments(0)
    val st2 = bd.property("beanArray").visibleType.arguments(0)
    assertEquals(st1, st2)
  }

  @Test
  def testCyclicBeanProperty {
    val bd = (descriptorOf[CyclicTestBean] rewrite propertyRules {
      case p @ PropertyDescriptor("cyclicRef", _) => p.withName("renamed")
    }).asInstanceOf[BeanDescriptor]

    //bd.properties foreach {p => println(p.toString)}

    def checkDeepCycle(metamodel: MetaModel, counter: Int) {
      if (counter > 0) {
        assertHasProperty(metamodel, "renamed")
        val renamed = metamodel.asInstanceOf[BeanDescriptor].property("renamed")
        checkDeepCycle(renamed.typeMetaModel, counter - 1)
      }
    }
    checkDeepCycle(bd, 10)
    checkDeepCycle(bd.property("renamed").typeMetaModel, 10)
    checkDeepCycle(bd.property("cyclicSet").typeMetaModel.asInstanceOf[ContainerMetaModel].elementMetaModel, 10)
    checkDeepCycle(bd.property("cyclicArray").typeMetaModel.asInstanceOf[ContainerMetaModel].elementMetaModel, 10)
    checkDeepCycle(bd.property("cyclicMap").typeMetaModel.asInstanceOf[ContainerMetaModel].
        elementMetaModel.asInstanceOf[BeanDescriptor].property("_2").
        typeMetaModel.asInstanceOf[ContainerMetaModel].
        elementMetaModel, 10)

    val st1 = bd.property("renamed").visibleType
    val st2 = bd.property("cyclicMap").visibleType.arguments(0).arguments(1).arguments(0)
    assertEquals(st1, st2)

//    val bd1 = st1.asInstanceOf[BeanType].beanDescriptor
//    val bd2 = st2.asInstanceOf[BeanType].beanDescriptor
//    assertEquals(bd1, bd2)
  }

  @Test
  def testCyclicBean {
    val bd = (descriptorOf[CyclicTestBean] rewrite metamodelRules {
      case bd: BeanDescriptor => bd.withoutProperty("cyclicSet")
    }).asInstanceOf[BeanDescriptor]

    //bd.properties foreach {p => println(p.toString)}

    def checkDeepCycle(metamodel: MetaModel, counter: Int) {
      if (counter > 0) {
        assertHasNoProperty(metamodel, "cyclicSet")
        val renamed = metamodel.asInstanceOf[BeanDescriptor].property("cyclicRef")
        checkDeepCycle(renamed.typeMetaModel, counter - 1)
      }
    }
    
    checkDeepCycle(bd, 10)
    checkDeepCycle(bd.property("cyclicRef").typeMetaModel, 10)
    checkDeepCycle(bd.property("cyclicArray").typeMetaModel.asInstanceOf[ContainerMetaModel].elementMetaModel, 10)
    checkDeepCycle(bd.property("cyclicMap").typeMetaModel.asInstanceOf[ContainerMetaModel].
        elementMetaModel.asInstanceOf[BeanDescriptor].property("_2").
        typeMetaModel.asInstanceOf[ContainerMetaModel].
        elementMetaModel, 10)

    val st1 = bd.property("cyclicRef").visibleType
    val st2 = bd.property("cyclicMap").visibleType.arguments(0).arguments(1).arguments(0)
    assertEquals(st1, st2)

//    val bd1 = st1.asInstanceOf[BeanType].beanDescriptor
//    val bd2 = st2.asInstanceOf[BeanType].beanDescriptor
//    assertEquals(bd1, bd2)
  }

  @Test
  def testScalaTypeRewrite {
    val rules = propertyRules {
      case p @ PropertyDescriptor(_, t) if t.erasure == classOf[java.util.UUID] =>
        p /*.convertValue(
          { uuid: java.util.UUID => (uuid.getMostSignificantBits(), uuid.getLeastSignificantBits()) },
          { bits: (Long, Long) => new java.util.UUID(bits._1, bits._2) })*/
    }

    val scalaType = descriptorOf[ObjectGraph] rewrite rules
  }

  private def assertHasProperty(metamodel: MetaModel, propertyName: String) {
    metamodel match {
      case bd: BeanDescriptor =>
        //bd.properties foreach {p => println(p.toString)}
        assertFalse(bd.propertyOption(propertyName).isEmpty)
      case _ => fail("BeanDescriptor expected, %s found".format(metamodel))
    }
  }

  private def assertHasNoProperty(metamodel: MetaModel, propertyName: String) {
    metamodel match {
      case bd: BeanDescriptor =>
        //bd.properties foreach {p => println(p.toString)}
        assertTrue(bd.propertyOption("mutable").isEmpty)
      case _ => fail("BeanDescriptor expected, %s found".format(metamodel))
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

  class ObjectGraph {
    var one: One = _
    var two: Two = _
  }

  class One {
    var two: Two = _
  }

  class Two {
    var one: One = _
  }
}