package org.scalabeans
package test

import org.junit.{Assert, Test}

class CaseClassTest {
  @Test
  def testCreate {
    val bd = Preamble.descriptorOf[CaseClassTestBean]
    val t = bd.newInstance().asInstanceOf[CaseClassTestBean]
    Assert.assertEquals("aa", t.street)
  }
}

case class CaseClassTestBean(var street: String = "aa")