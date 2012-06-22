package org.scalastuff.scalabeans.testmono

import org.scalastuff.scalabeans.Preamble._
import org.scalatest.FlatSpec
import org.scalastuff.util.Converter
import org.scalatest.matchers.ShouldMatchers

trait ObjectGraphMonoBehaviours extends MonoBehaviours with ShouldMatchers { self: FlatSpec =>

  def objectGraphMono[B]() {
    val wrapper = new testbeans.ObjectGraph
    wrapper.one = new testbeans.One
    wrapper.two = new testbeans.Two
    wrapper.one.two = wrapper.two
    wrapper.two.one = wrapper.one

    it should "reproduce references" in {
      checkInverse(wrapper) { (b, deser) =>
        deser.two should be theSameInstanceAs (deser.one.two)
        deser.one should be theSameInstanceAs (deser.two.one)
      }

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