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

import org.junit.{Test, Assert}

class ImmutableTest {
  import TestFormat._

  @Test
  def testSimple {
    checkFormats(() => new ImmutableTestBean())
  }
}

case class Immutable(s: String, i: Int)

class ImmutableTestBean extends TestBean[ImmutableTestBean] {
  var immutableBean = Immutable("", 0)
  var tuple = (0, "")

  def set1() = {
    immutableBean = Immutable("whatever", 10)
    tuple = (20, "tuple")
    this
  }

  def assertEquals(other: ImmutableTestBean) {
    Assert.assertEquals(immutableBean, other.immutableBean)
    Assert.assertEquals(tuple, other.tuple)
  }
}