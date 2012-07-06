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

package org.scalastuff.scalabeans

import Preamble._
import org.junit.Test
import org.junit.Assert._

class BeanIntrospectorTest {

  @Test
  def testConstructorParameters() {
    val bd = descriptorOf[NotConstructorPropertyBean]

    assertTrue(bd.property("i").isInstanceOf[ConstructorParameter])
    assertFalse(bd.property("s").isInstanceOf[ConstructorParameter])
  }

  @Test
  def testCanCreateDescriptorOfAbstractClass() {
    val bd = descriptorOf[AbstractBean]

    assertNotNull(bd)
    assertEquals(1, bd.properties.size)
    assertNotNull(bd.property("i"))
  }

  @Test
  def testOverrideInConstructorParam() {
    val bd = descriptorOf[OverrideSuperPropertyBean]

    assertNotNull(bd)
    assertEquals(1, bd.properties.size)
    assertNotNull(bd.property("i"))
    assertTrue(bd.property("i").isInstanceOf[ConstructorParameter])
    assertEquals(scalaTypeOf[Seq[Int]], bd.property("i").underlyingType)
  }
}

abstract class AbstractBean(val i: Traversable[Int])

class NotConstructorPropertyBean(_s: String, _i: Traversable[Int]) extends AbstractBean(_i) {
  val s = _s + "x"
}

class OverrideSuperPropertyBean(override val i: Seq[Int]) extends AbstractBean(i)