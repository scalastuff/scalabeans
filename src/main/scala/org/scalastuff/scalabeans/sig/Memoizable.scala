/**
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
package org.scalastuff.scalabeans.sig
import scala.collection.mutable.HashMap

object Memoizable {
  def apply[A, B](pf: PartialFunction[A, B]) = new PartialFunction[A, B] {
    private[this] val memo = new HashMap[A, B]

    def isDefinedAt(x: A): Boolean = pf.isDefinedAt(x)
    def apply(x: A): B = memo.getOrElseUpdate(x, pf(x))
  }
  
  def apply[A, B](f: A => B): A => B = new Function[A, B] {
    private[this] val memo = new HashMap[A, B]
    def apply(x: A): B = memo.getOrElseUpdate(x, f(x))
  }
}