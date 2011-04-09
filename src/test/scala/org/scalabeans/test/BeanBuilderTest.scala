/*
 * Copyright (c) 2011 ScalaStuff.org (joint venture of Alexander Dvorkovyy and Ruud Diterwich)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package org.scalabeans
package test

import Preamble._
import org.junit.Test
import org.junit.Assert._

class BeanBuilderTest {

  @Test(expected = classOf[IllegalArgumentException])
  def testCannotDeserialize() {
    val bd = descriptorOf[CannotDeserializeBean]
    val builder = bd.newBuilder
  }

  @Test
  def testValsAndVarsInConstructor() {
    val bd = descriptorOf[ValsAndVarsInConstructorBean]
    val builder = bd.newBuilder

    assertEquals(2, builder.constructorParams.size)
    assertEquals(0, builder.mutableProperties.size)
    assertEquals(-1, builder.lastMutablePropertyIndex)
  }

  @Test
  def testValsAndVars() {
    val bd = descriptorOf[SubclassValsAndVarsInConstructorBean]
    val builder = bd.newBuilder

    assertEquals(2, builder.constructorParams.size)
    assertEquals(2, builder.mutableProperties.size)
    assertEquals(1, builder.lastMutablePropertyIndex)

    builder.set(bd("p1"), "value1")
    builder.set(bd("p2"), "value2")
    builder.set(bd("p3"), "value3")
    builder.set(bd("p4"), 10)

    val result = builder.result().asInstanceOf[SubclassValsAndVarsInConstructorBean]

    assertEquals(result.p1, "value1")
    assertEquals(result.p2, "value2")
    assertEquals(result.p3, "value3")
    assertEquals(result.p4, 10)
  }

  @Test
  def testCreateImmutable() {
    val bd = descriptorOf[ValsAndVarsInConstructorBean]
    val builder = bd.newBuilder

    builder.set(bd("p1"), "value1")
    builder.set(bd("p2"), "value2")

    val result = builder.result().asInstanceOf[ValsAndVarsInConstructorBean]

    assertEquals(result.p1, "value1")
    assertEquals(result.p2, "value2")
  }

  @Test
  def testIgnoreConsts() {
    val bd = descriptorOf[IgnoreConstsBean]
    val builder = bd.newBuilder

    assertEquals(0, builder.constructorParams.size)
    assertEquals(0, builder.mutableProperties.size)
    assertEquals(-1, builder.lastConstructorParamIndex)
    assertEquals(-1, builder.lastMutablePropertyIndex)

    val result = builder.result()
    assertNotNull(result)
    assertTrue(result.isInstanceOf[IgnoreConstsBean])
  }
}

class CannotDeserializeBean(_s: String) {
  val s = _s + "x"
}

class ValsAndVarsInConstructorBean(val p1:String, var p2: String)
class SubclassValsAndVarsInConstructorBean(p1:String, p2: String) extends ValsAndVarsInConstructorBean(p1, p2) {
  var p3 = ""
  var p4 = 0
}

class IgnoreConstsBean {
  val x = "x"
  final val y = "y"
}