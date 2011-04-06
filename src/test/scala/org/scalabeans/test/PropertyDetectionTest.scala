package org.scalabeans.test

import org.scalabeans.Preamble._
import org.scalabeans._
import org.junit.Test


class TestBean {

	var property1 : String = ""
	val property2 : String = ""
	private var property3 : String = ""
	private val property4 : String = ""
}

class PropertyDetectionTest {
  val AddressType = scalaTypeOf[Address]

  @Test
  def testMe {
    val desc = descriptorOf[TestBean]

    for (p <- desc.properties) {
      print(p + ": " + p.getClass.getName)
      p.scalaType match {
        case OptionType(IntType) => println(" Option of Int")
        case AddressType => println("ADDRESS")
        case OptionType(AddressType) => println("OPTIONAL ADDRESS")
        //    			case s : scalaTypeOf[Address] => println("ADDRESS")
        case r@StringType => println(" string2: " + r.getClass.getName)
        case r@AnyRefType() => println(" anyref: " + r.erasure)
        case r: AnyRefType => println(" anyref: " + r.erasure)
        case _ => println("  Unknown type")
      }
    }

  }

  def manifestTest[A](implicit manifest: Manifest[A]) {
    println("type was: " + manifest.erasure)
  }
}
