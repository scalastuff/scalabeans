package org.scalastuff.scalabeans.types

import org.junit.{ Assert, Test }
import org.junit.Assert._
import org.scalastuff.scalabeans.Preamble._

class ScalaTypeTest {

  @Test
  def testEquality() {
    val st1 = scalaTypeOf[(Float, Float)]
    val st2 = scalaTypeOf[(Float, Float)]
    assertEquals(st1, st2)
  }
}