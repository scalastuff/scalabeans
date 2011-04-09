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

package org.scalabeans.stuff

import collection.mutable.Buffer
import org.junit.{Assert, Test}
import com.dyuproject.protostuff.{ProtobufIOUtil, GraphIOUtil, LinkedBuffer}
import org.scalabeans.Enum

class CollectionsTest {
  val linkedBuffer = LinkedBuffer.allocate(512)

  @Test
  def testGraph() {
    val schema = MirrorSchema.schemaOf[Person]

    def checkSerDeser(person: Person) = {
      linkedBuffer.clear()
      val buffer:Array[Byte] = GraphIOUtil.toByteArray(person, schema, linkedBuffer)
      println("Person: " + (buffer mkString " "))
      val deser1 = new Person()
      GraphIOUtil.mergeFrom(buffer, deser1, schema)
      person.assertEquals(deser1)
      deser1
    }

    checkSerDeser(new Person())
    val deser1 = checkSerDeser(new Person().set1())
    Assert.assertSame(deser1.homeAddress, deser1.adresses(1))
    Assert.assertSame(deser1.postAddress, deser1.adresses(0))
  }

  @Test
  def testProtobuf() {
    val schema = MirrorSchema.schemaOf[Person]

    def checkSerDeser(person: Person) = {
      linkedBuffer.clear()
      val buffer:Array[Byte] = ProtobufIOUtil.toByteArray(person, schema, linkedBuffer)
      println("Person: " + (buffer mkString " "))
      val deser1 = new Person()
      ProtobufIOUtil.mergeFrom(buffer, deser1, schema)
      person.assertEquals(deser1)
    }

    checkSerDeser(new Person())
    checkSerDeser(new Person().set1())
  }
}

class Person {
  var homeAddress: Address = new Address
  var adresses: List[Address] = Nil
  var postAddress: Address = new Address
  var names: Buffer[String] = Buffer()
  var adressPerType = Map[AddressType, Address]()
  var tags = Seq[Option[String]]()

  def set1() = {
    val addr1 = new Address
    addr1.street = "Street 1"

    val addr2 = new Address
    addr2.street = "Street 2"

    val addr3 = new Address
    addr3.street = "Street 3"

    adresses = List(addr1, addr2, addr3)
    names = Buffer("Juan", "Maria")
    homeAddress = addr2
    postAddress = addr1
    adressPerType = Map(
      (AddressType.Home -> addr1),
      (AddressType.Post -> addr2)
    )

    tags = Seq(None, Some("tag"))
    this
  }

  def assertEquals(other: Person) {
    Assert.assertEquals(homeAddress, other.homeAddress)
    Assert.assertEquals(adresses, other.adresses)
    Assert.assertEquals(postAddress, other.postAddress)
    Assert.assertEquals(names, other.names)
    Assert.assertEquals(adressPerType, other.adressPerType)
  }
}

class AddressType private ()
object AddressType extends Enum[AddressType] {
  val Home = new AddressType name "H"
  val Post = new AddressType name "P"
}

case class Address(var street: String = "")