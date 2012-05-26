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

class Person(val name: String = "", var address: Address) {
  var height: Option[Float] = None
  var birthDate: Option[java.util.Date] = None
  var values = Seq[Int]()

  @Deprecated
  private[this] var xxxx = ""
  private[this] val yyyy = ""
}

trait WorkingPerson {
  var income: Option[Float] = None
  // ?? option type parameter is ommited by compiler ??
  var employer: String
  // this is not visible at all via reflection!
}

trait FriendlyPerson {
  def yearlyDonations: Double = 0

  def yearlyDonations_=(donations: Double)
}

abstract class GreatPerson extends Person("Charles", new Address) with WorkingPerson with FriendlyPerson {
  var invoiceAddress: Option[Address] = None
  var specialness: Option[Int]
}

class Address