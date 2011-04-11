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

import com.dyuproject.protostuff.{LinkedBuffer, GraphIOUtil}
import org.junit.{Assert, Test}

class GraphTest {
  val linkedBuffer = LinkedBuffer.allocate(512)

  @Test
  def testSimpleCase() {
    val wrapper = new Wrapper
    wrapper.one = new One
    wrapper.two = new Two
    wrapper.one.two = wrapper.two
    wrapper.two.one = wrapper.one

    val schema = MirrorSchema.schemaOf[Wrapper]

    linkedBuffer.clear()
    val buffer = GraphIOUtil.toByteArray(wrapper, schema, linkedBuffer)
    val deser = new Wrapper
    GraphIOUtil.mergeFrom(buffer, deser, schema)

    Assert.assertSame(deser.two, deser.one.two)
    Assert.assertSame(deser.one, deser.two.one)
  }
}

class Wrapper {
  var one: One = _
  var two: Two = _
}

class One {
  var two: Two = _
}

class Two {
  var one: One = _
}