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

package org.scalastuff.proto.value

import org.junit.{Test, Assert}
import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.scalabeans._
import org.scalastuff.scalabeans.types._

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