package org.scalastuff.scalabeans

import org.junit.Test
import org.junit.Assert._
import Preamble._

class BeanDescriptorTest {
  @Test
  def testFindProperties() {
    val bd = descriptorOf[TestBean]
    
    val property1 = bd("property1")
    assertNotNull(property1)
    assertEquals("property1", property1.name)
    
    val property2 = bd.property("property2").get
    assertNotNull(property2)
    assertEquals("property2", property2.name)
  }
  
  @Test
  def testPropertyGet() {
    val bd = descriptorOf[TestBean]
    
    val testBean = new TestBean { override val property2 = "value" }
    val value = bd.get(testBean, "property2")
    assertEquals("value", value)
  }
  
  @Test
  def testPropertySet() {
    val bd = descriptorOf[TestBean]
    
    val testBean = new TestBean
    val value = bd.set(testBean, "property1", "value")
    assertEquals("value", testBean.property1)
  }
}