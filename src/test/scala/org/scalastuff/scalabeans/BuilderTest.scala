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

import types._
import org.junit.{Assert, Test}
import org.junit.Assert._
import collection.mutable.{Buffer, HashMap}

class BuilderTest {
  @Test
  def testEmptySeq = checkEmptyBuilderForType[Seq[_]]
  @Test
  def testEmptySet = checkEmptyBuilderForType[Set[_]]
  @Test
  def testEmptyMap = checkEmptyBuilderForType[Map[_, _]]

  @Test
  def testSeq = checkBuilderForType[Seq[_]]
  @Test
  def testSet = checkBuilderForType[Set[_]]
  @Test
  def testMap = checkMapBuilderForType[Map[_, _]]

  @Test
  def testBuffer = checkBuilderForType[Buffer[_]]

  @Test
  def testHashMap = checkMapBuilderForType[HashMap[_, _]]

  def checkEmptyBuilderForType[T](implicit mf: Manifest[T]) {
    val st = ScalaType.scalaTypeOf[T]
    st match {
      case t: TraversableType =>
        assertEquals(mf.erasure, t.erasure)
        val builder = t.newBuilder.get.apply
        val empty = builder.result
        assertTrue(mf.erasure.isAssignableFrom(empty.getClass))
        assertEquals(0, empty.size)

      case _ => fail("TraversableType expected")
    }
  }

  def checkBuilderForType[T](implicit mf: Manifest[T]) {
    val st = ScalaType.scalaTypeOf[T]
    st match {
      case t: TraversableType =>
        assertEquals(mf.erasure, t.erasure)
        val builder = t.newBuilder.get.apply
        builder += ""
        builder += 1
        builder += new java.util.Date()
        val result = builder.result
        assertTrue(mf.erasure.isAssignableFrom(result.getClass))
        assertEquals(3, result.size)

      case _ => fail("TraversableType expected")
    }
  }

  def checkMapBuilderForType[T](implicit mf: Manifest[T]) {
    val st = ScalaType.scalaTypeOf[T]
    st match {
      case t: TraversableType =>
        assertEquals(mf.erasure, t.erasure)
        val builder = t.newBuilder.get.apply
        builder += (("a", 1))
        builder += (("b", 2))
        builder += (("c", 3))
        val result = builder.result
        assertTrue(mf.erasure.isAssignableFrom(result.getClass))
        assertEquals(3, result.size)

      case _ => fail("TraversableType expected")
    }
  }
}