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
package org.scalastuff.proto

import org.junit.{ Test, Assert }

class BeanReaderWriterTest {
  import TestFormat._

  @Test
  def testVarConstructorParameter {
    checkFormats(() => new VarConstructorParameterBean("a"))
  }

  @Test
  def testWrappedTypes {
    val person = new Person().set1()
    checkSerDeserFormats(person.adresses)(Assert.assertEquals(_, _))
    checkSerDeserFormats(person.names)(Assert.assertEquals(_, _))
    checkSerDeserFormats(person.adressPerType)(Assert.assertEquals(_, _))
    checkSerDeserFormats(person.tags)(Assert.assertEquals(_, _))
//	this doesn't work because of type erasure in array type, also in Manifest
//    checkSerDeserFormats(person.tagsArray)({ (expected, tested) => 
//      Assert.assertArrayEquals(expected.asInstanceOf[Array[AnyRef]], tested.asInstanceOf[Array[AnyRef]])
//    })
    checkSerDeserFormats(person.primitiveArray)(Assert.assertArrayEquals(_, _))
  }
}

class VarConstructorParameterBean(var s: String) extends TestBean[VarConstructorParameterBean] {
  def set1() = {
    s = "whatever"
    this
  }

  def assertEquals(other: VarConstructorParameterBean) {
    Assert.assertEquals(s, other.s)
  }
}