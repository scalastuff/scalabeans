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
