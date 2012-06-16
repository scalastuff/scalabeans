package org.scalastuff.scalabeans

import org.scalastuff.scalabeans.Preamble._
import org.scalatest.FlatSpec
import org.scalastuff.util.Converter
import org.scalatest.matchers.ShouldMatchers

trait ObjectGraphConverterBehaviours extends ShouldMatchers { self: FlatSpec =>

  type ObjectGraph = testbeans.ObjectGraph
  
  val wrapper = new ObjectGraph
  wrapper.one = new testbeans.One
  wrapper.two = new testbeans.Two
  wrapper.one.two = wrapper.two
  wrapper.two.one = wrapper.one

  def objectGraphConverter[B](conv: Converter[ObjectGraph, B]) {
    it should "reproduce references" in {
      val b = conv.to(wrapper)
      val deser = conv.from(b)
      
      deser.two should be theSameInstanceAs (deser.one.two)
      deser.one should be theSameInstanceAs (deser.two.one)
    }
  }
}

package testbeans {
  class ObjectGraph {
    var one: One = _
    var two: Two = _
  }

  class One {
    var two: Two = _
  }

  class Two {
    var one: One = _
  }
}