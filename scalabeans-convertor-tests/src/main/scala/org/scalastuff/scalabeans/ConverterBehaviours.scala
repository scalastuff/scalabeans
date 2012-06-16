package org.scalastuff.scalabeans

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalastuff.util.Converter

trait ConverterBehaviours extends ShouldMatchers { self: FlatSpec =>
  def symmetricConverter[A, B](convertor: Converter[A, B], testValues: Iterable[A]) {
    it should "be symmertic" in {
      for (testVal <- testValues) {
        val b = convertor.to(testVal)
        val a = convertor.from(b)

        a should equal(testVal)
      }
    }
  }
}