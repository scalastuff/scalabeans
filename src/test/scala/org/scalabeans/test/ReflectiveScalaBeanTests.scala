package org.scalabeans.test

import org.scalabeans.Preamble._
import org.scalabeans._
import org.junit.Test

//@RunWith(classOf[JUnitRunner])
class IntrospectionScalaBeanTests {
  val AddressType = scalaTypeOf[Address]

  @Test
  def testMe {
    val bean = descriptorOf[GreatPerson]
    println(bean.property("xxxx").get.findAnnotation[Deprecated])

    for (p <- bean.properties) {
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
