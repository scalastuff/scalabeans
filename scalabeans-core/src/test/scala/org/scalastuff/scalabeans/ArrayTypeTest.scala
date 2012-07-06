package org.scalastuff.scalabeans

import Preamble._
import types._
import org.junit.Assert._
import org.junit.{ Test, Ignore }

class ArrayTypeTest {

  @Test
  def testManifest() {
    checkPrimitiveArrayType(
      scalaTypeOf[Array[Float]])
  }

  @Ignore("Scala 2.8.1 Manifest bug")
  @Test
  def testGenericTypeManifest() {
    checkGenericArrayType(
      scalaTypeOf[Array[(Float, Float)]])
  }

  @Test
  def testBeanPropertyGenericType() {
    checkGenericArrayType(
      descriptorOf[ArrayTestBean].property("arr1").visibleType)
  }

  @Test
  def testBeanPropertyType() {
    checkPrimitiveArrayType(
      descriptorOf[ArrayTestBean].property("arr2").visibleType)
  }

  @Test
  def testNewBuilder() {
    val scalaType = scalaTypeOf[Array[Int]] match { case at:ArrayType => at }
    val builder = scalaType.newArrayBuilder[Any]
//    val mf = manifest[Array[Float]]
//    val builder = mf.newArrayBuilder
//    val arr = builder.result
    assertArrayEquals(Array.ofDim[Int](0), builder.result.asInstanceOf[Array[Int]])
  }

  def checkPrimitiveArrayType(scalaType: ScalaType) = scalaType match {
    case ArrayType(ct) =>
      assertEquals(FloatType, ct)
    case _ => fail("pattern match expected")
  }

  def checkGenericArrayType(scalaType: ScalaType) = scalaType match {
    case ArrayType(ct) =>
      assertEquals(scalaTypeOf[(Float, Float)], ct)
    case _ => fail("pattern match expected")
  }
}

class ArrayTestBean {
  var arr1: Array[Tuple2[Float, Float]] = null
  var arr2: Array[Float] = null
}