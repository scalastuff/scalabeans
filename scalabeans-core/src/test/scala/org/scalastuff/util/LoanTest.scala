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
package org.scalastuff.util

import Loan._
import org.junit.Test
import org.junit.Assert._

class LoanTest {

  @Test
  def testOpen2 {
    val opened = Array.ofDim[Boolean](2)
    try {
      val (res1, res2) = open(new TestResource(0, opened), exceptionOnOpen())
      fail("OpenResourceException expected")
    } catch {
      case _: OpenResourceException => // yes, this is what we wanted
    }
    assertFalse(opened contains true)
  }

  @Test
  def testOpen3 {
    val opened = Array.ofDim[Boolean](2)
    try {
      open(new TestResource(0, opened), new TestResource(1, opened), exceptionOnOpen())
      fail("OpenResourceException expected")
    } catch {
      case _: OpenResourceException => // yes, this is what we wanted
    }
    assertFalse(opened contains true)
  }

  @Test
  def testOpen4 {
    val opened = Array.ofDim[Boolean](3)
    try {
      open(new TestResource(0, opened), new TestResource(1, opened), new TestResource(2, opened), exceptionOnOpen())
      fail("OpenResourceException expected")
    } catch {
      case _: OpenResourceException => // yes, this is what we wanted
    }
    assertFalse(opened contains true)
  }

  @Test
  def testOpen5 {
    val opened = Array.ofDim[Boolean](4)
    try {
      open(new TestResource(0, opened), new TestResource(1, opened), new TestResource(2, opened), new TestResource(3, opened), exceptionOnOpen())
      fail("OpenResourceException expected")
    } catch {
      case _: OpenResourceException => // yes, this is what we wanted
    }
    assertFalse(opened contains true)
  }

  @Test
  def testOpen53 {
    val opened = Array.ofDim[Boolean](4)
    try {
      open(new TestResource(0, opened), new TestResource(1, opened), exceptionOnOpen(), new TestResource(2, opened), new TestResource(3, opened))
      fail("OpenResourceException expected")
    } catch {
      case _: OpenResourceException => // yes, this is what we wanted
    }
    assertFalse(opened contains true)
  }

  @Test
  def testOpenSuccess {
    val opened = Array.ofDim[Boolean](5)
    open(new TestResource(0, opened), new TestResource(1, opened), new TestResource(2, opened), new TestResource(3, opened), new TestResource(4, opened))
    assertFalse(opened contains false)
  }

  @Test
  def testLoanWithoutException {
    val opened = Array.ofDim[Boolean](3)
    val (res1, res2, res3) = open(new TestResource(0, opened) {}, new TestResource(1, opened) {}, new TestResource(2, opened) {})
    assertFalse(opened contains false)

    loan(res1, res2, res3) {

    }
    assertFalse(opened contains true)
  }

  @Test
  def testLoanWithException {
    val opened = Array.ofDim[Boolean](3)
    val (res1, res2, res3) = open(new TestResource(0, opened) {}, new TestResource(1, opened) {}, new TestResource(2, opened) {})
    assertFalse(opened contains false)

    try {
      loan(res1, res2, res3) {
        throw new ExecutionBlockException
      }
    } catch {
      case _:ExecutionBlockException => // yes, this is what we wanted
    }
    assertFalse(opened contains true)
  }

  @Test
  def testLoanWithExceptionOnClose {
    val opened = Array.ofDim[Boolean](3)
    val (res1, res2, res3) = open(new TestResource(0, opened) {}, new TestCloseExceptionResource(1, opened) {}, new TestResource(2, opened) {})
    assertFalse(opened contains false)

    try {
      loan(res1, res2, res3) {

      }
    } catch {
      case _:CloseResourceException => // yes, this is what we wanted
    }
    assertFalse(opened contains true)
  }

  @Test
  def testLoanWithExceptionAndExceptionOnClose {
    val opened = Array.ofDim[Boolean](3)
    val (res1, res2, res3) = open(new TestResource(0, opened) {}, new TestCloseExceptionResource(1, opened) {}, new TestResource(2, opened) {})
    assertFalse(opened contains false)

    try {
      loan(res1, res2, res3) {
        throw new ExecutionBlockException
      }
    } catch {
      case _:ExecutionBlockException => // yes, this is what we wanted
    }
    assertFalse(opened contains true)
  }

  class TestResource(index: Int, opened: Array[Boolean]) {
    opened(index) = true

    def close() {
      opened(index) = false
    }
  }

  class TestCloseExceptionResource(index: Int, opened: Array[Boolean]) {
    opened(index) = true

    def close() {
      opened(index) = false
      throw new CloseResourceException
    }
  }

  class OpenResourceException extends RuntimeException
  class CloseResourceException extends RuntimeException
  class ExecutionBlockException extends RuntimeException
  def exceptionOnOpen(): TestResource = throw new OpenResourceException
}