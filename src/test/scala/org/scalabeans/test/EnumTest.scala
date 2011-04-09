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

package org.scalabeans
package test

import org.junit.{Assert, Test}
import org.junit.Assert._
import Preamble._

class Color private ()
object Color extends Enum[Color] {
	val blue = new Color ordinal 4 
	val red = new Color
	val yellow = new Color name "yel" ordinal 5
}

case class EnumTestBean(var color : Color = Color.red)

class EnumTest {
	@Test
	def testEnumValues {
		println("Enum value names: " + Color.values.map(_.name))
		assertEquals(Seq("blue", "yel", "red"), Color.values.map(_.name))
		println("Enum value ordinals: " + Color.values.map(_.ordinal))
		assertEquals(Seq(4, 5, 6), Color.values.map(_.ordinal))
	}
	
	@Test
	def testEnumMatch {
		val value = Color.red
		value match {
			case Color.red => println("Red Color")
			case _ => fail("Expected red")
		}
		value match {
		case c : Color => println("Color: " + Color.nameOf(c))
		case _ => fail("Expected red")
		}
	}
	
  @Test
  def testEnumOf {
  	val someValue : AnyRef = Color.red
  	val enum = Enum.enumOf(someValue)
  	println("Enum: " + enum)
  	assertEquals(Some(Color), enum)
  }
  
  @Test
  def testEnumType {
    val bd = descriptorOf[EnumTestBean]
    val pd = bd.property("color").get
    pd.scalaType match {
    	case EnumType(enum) => 
    		println("Enum: " + enum)
    		assertEquals(Color, enum)
        assertEquals("red", enum.nameOf(Color.red))
    	case _ => fail("Expected enum type")
    }
  }
}
