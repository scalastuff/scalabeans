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

/**
 * Implements Scala Loan design pattern.
 *
 * Works with any resources implementing close() method.
 *
 * Usage example:
 * {{{
 * import org.scalastuff.util.Loan._
 *
 * val (res1, res2) = open(new InputStream(..), new OutputStream(..))
 * loan(res1, res2) {
 *   res1.read(..)
 *   res2.write(..)
 *   ...
 * }
 * }}}
 */
object Loan {
  type Closeable = { def close() }

  /**
   * Safely opens resources.
   *
   * Already opened resources get closed if exception is thrown.
   */
  def open[R1 <: Closeable, R2 <: Closeable](or1: => R1, or2: => R2): (R1, R2) = {
    val r1 = or1
    val r2 = closeOnException(r1) { or2 }
    (r1, r2)
  }

  /**
   * Safely opens resources.
   *
   * Already opened resources get closed if exception is thrown.
   */
  def open[R1 <: Closeable, R2 <: Closeable, R3 <: Closeable](or1: => R1, or2: => R2, or3: => R3): (R1, R2, R3) = {
    val r1 = or1
    val (r2, r3) = closeOnException(r1) { open(or2, or3) }
    (r1, r2, r3)
  }

  /**
   * Safely opens resources.
   *
   * Already opened resources get closed if exception is thrown.
   */
  def open[R1 <: Closeable, R2 <: Closeable, R3 <: Closeable, R4 <: Closeable](or1: => R1, or2: => R2, or3: => R3, or4: => R4): (R1, R2, R3, R4) = {
    val r1 = or1
    val (r2, r3, r4) = closeOnException(r1) { open(or2, or3, or4) }
    (r1, r2, r3, r4)
  }

  /**
   * Safely opens resources.
   *
   * Already opened resources get closed if exception is thrown.
   */
  def open[R1 <: Closeable, R2 <: Closeable, R3 <: Closeable, R4 <: Closeable, R5 <: Closeable](or1: => R1, or2: => R2, or3: => R3, or4: => R4, or5: => R5): (R1, R2, R3, R4, R5) = {
    val r1 = or1
    val (r2, r3, r4, r5) = closeOnException(r1) { open(or2, or3, or4, or5) }
    (r1, r2, r3, r4, r5)
  }

  /**
   * Ensures all resources are closed after given code block is finished.
   */
  def loan[A](rs: Closeable*)(f: => A): A = {
    try {
      f
    } finally {
      rs foreach close
    }
  }

  /**
   * Closes resource. Exceptions are printed to standard output and not re-thrown.
   */
  def close(r: Closeable) {
    try {
      r.close()
    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
  }

  /**
   * Closes resource if exception is thrown. Exception gets re-thrown after resource is closed.
   *
   * Usage:
   *
   * {{{
   * closeOnException(res) {
   *   ...
   * }
   * }}}
   */
  def closeOnException[A](r: Closeable)(f: => A) = {
    try {
      f
    } catch {
      case e: Exception =>
        close(r)
        throw e
    }
  }
}