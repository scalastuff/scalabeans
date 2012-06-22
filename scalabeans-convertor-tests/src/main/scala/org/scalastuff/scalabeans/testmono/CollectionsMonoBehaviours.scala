package org.scalastuff.scalabeans.testmono
import scala.collection.mutable.Buffer
import org.scalastuff.scalabeans.Enum
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import java.util.Arrays

trait CollectionsMonoBehaviours extends MonoBehaviours with ShouldMatchers { self: FlatSpec =>
  import collectionproperties._

  def inverseForCollections() {
    it should "support beans with collection types" in {
      checkInverse(new Person(), new Person().set1())()
    }

    it should "support immutable collection types" in {
      checkInverse(new Person().adresses, new Person().set1().adresses)()
    }

    it should "support mutable collection types" in {
      checkInverse(new Person().names, new Person().set1().names)()
    }

    it should "support collection types with elements of type Option" in {
      checkInverse(new Person().tags, new Person().set1().tags)()
    }

    //	this doesn't work because of type erasure in array type, also in Manifest
    ignore should "support arrays with elements of type Option" in {
      checkInverse(new Person().tagsArray, new Person().set1().tagsArray)()
    }

    it should "support arrays with elements of primitive types" in {
      checkInverse(new Person().primitiveArray, new Person().set1().primitiveArray)()
    }
    
    it should "support nested containers" in {
      checkInverse(new NestedInlinedTestBean(), new NestedInlinedTestBean().set1())()
    }
  }
}

package collectionproperties {
  class Person(
    var homeAddress: Address = Address(),
    var adresses: List[Address] = Nil,
    var postAddress: Address = Address(),
    var names: Buffer[String] = Buffer(),
    var adressPerType: Map[AddressType, Address] = Map[AddressType, Address](),
    var tags: Seq[Option[String]] = Seq[Option[String]](),
    var tagsArray: Array[Option[String]] = Array.ofDim[Option[String]](0),
    var primitiveArray: Array[Int] = Array.ofDim[Int](0)) {

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
        (AddressType.Post -> addr2))

      tags = Seq(None, Some("tag"))
      tagsArray = tags.toArray
      primitiveArray = Array(10, 20, 5)
      this
    }

    override def equals(obj: Any) = obj match {
      case other: Person =>
        homeAddress == other.homeAddress &&
          adresses == other.adresses &&
          postAddress == other.postAddress &&
          names == other.names &&
          adressPerType == other.adressPerType &&
          tags == other.tags &&
          tagsArray.sameElements(other.tagsArray) &&
          primitiveArray.sameElements(other.primitiveArray)
      case _ => false
    }
  }

  class AddressType private ()
  object AddressType extends Enum[AddressType] {
    val Home = new AddressType name "H"
    val Post = new AddressType name "P"
  }

  case class Address(var street: String = "")

  case class NestedInlinedTestBean(
    var lo: List[Option[String]]= List[Option[String]](),
    var ol: Option[List[String]] = None,
    var l3: List[List[List[String]]] = List[List[List[String]]]()) {

    def set1() = {
      lo = List(Some("str"), None, Some("other"))
      ol = Some(List("e1", "e2"))
      l3 = List(List(List("0-0-1", "0-0-2"), List("0-1-1", "0-1-2")))

      this
    }
  }

}