package org.scalabeans.test

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