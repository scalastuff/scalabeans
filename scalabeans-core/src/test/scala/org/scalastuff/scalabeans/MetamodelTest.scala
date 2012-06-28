package org.scalastuff.scalabeans

import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalastuff.scalabeans.types._
import Preamble._
import org.scalastuff.scalabeans.converters.Converter
import java.util.Date

@RunWith(classOf[JUnitRunner])
class MetamodelTest extends FlatSpec with ShouldMatchers {
  "Converted ValueMetamodel" should "contain correct ScalaTypes and conversion" in {
    val intMetamodel = metamodelOf[Int] rewrite metamodelRules {
      case mm @ Metamodel(IntType) => mm.convert(Converter[Int, String](_.toString, _.toInt))
    }
    intMetamodel.scalaType should equal(IntType)
    intMetamodel.converter.to(1) should be("1")
    intMetamodel.converter.from("2") should be(2)

    intMetamodel.isInstanceOf[ConvertedMetamodel] should be(true)
    val cv = intMetamodel.asInstanceOf[ConvertedMetamodel]
    cv.visibleMetamodel.scalaType should be(StringType)
  }

  "Converted ArrayMetamodel" should "contain correct ScalaTypes and conversion" in {
    val testMetamodel = metamodelOf[Array[Int]] rewrite metamodelRules {
      case mm @ Metamodel(IntType) => mm.convert(Converter[Int, String](_.toString, _.toInt))
    }
    testMetamodel.scalaType should equal(ArrayType(IntType))
    testMetamodel.converter.to(Array(1)) should be(Array("1"))
    testMetamodel.converter.from(Array("2")) should be(Array(2))

    testMetamodel.isInstanceOf[ConvertedMetamodel] should be(true)
    val cv = testMetamodel.asInstanceOf[ConvertedMetamodel]
    cv.visibleMetamodel.scalaType should be(ArrayType(StringType))
  }

  "ArrayMetamodel, converted twice" should "contain correct ScalaTypes and conversion" in {
    val testMetamodel = metamodelOf[Array[Int]] rewrite metamodelRules {
      case mm @ Metamodel(IntType) => mm.convert(Converter[Int, String](_.toString, _.toInt))
    } rewrite metamodelRules {
      case mm @ Metamodel(StringType) => mm.convert(Converter[String, Int](_.toInt, _.toString))
    }

    testMetamodel.scalaType should equal(ArrayType(IntType))
    testMetamodel.converter.to(Array(1)) should be(Array(1))
    testMetamodel.converter.from(Array(2)) should be(Array(2))

    testMetamodel.isInstanceOf[ConvertedMetamodel] should be(true)
    val cv = testMetamodel.asInstanceOf[ConvertedMetamodel]
    cv.visibleMetamodel.scalaType should be(ArrayType(IntType))
  }

  "ValueMetamodel converted to ContainerMetamodel" should "be rewritable" in {
    val testMetamodel = metamodelOf[Int] rewrite metamodelRules {
      case mm @ Metamodel(IntType) => mm.convert(Converter[Int, Option[String]]({ i => Some("x" + i.toString) }, _.get.substring(1).toInt))
    } rewrite metamodelRules {
      case mm @ Metamodel(StringType) => mm.convert(Converter[String, Int](_.substring(1).toInt, "y" + _.toString))
    }

    testMetamodel.scalaType should equal(IntType)
    testMetamodel.converter.to(1) should be(Some(1))
    testMetamodel.converter.from(Some(2)) should be(2)

    testMetamodel.isInstanceOf[ConvertedMetamodel] should be(true)
    val cv = testMetamodel.asInstanceOf[ConvertedMetamodel]
    cv.visibleMetamodel.scalaType should be(OptionType(IntType))
    cv.visibleMetamodel.isInstanceOf[ContainerMetamodel] should be(true)
  }

  "ValueMetamodel inside container converted to ContainerMetamodel" should "be rewritable" in {
    val testMetamodel = metamodelOf[Array[Int]] rewrite metamodelRules {
      case mm @ Metamodel(IntType) => mm.convert(Converter[Int, Option[String]]({ i => Some("x" + i.toString) }, _.get.toInt))
    } rewrite metamodelRules {
      case mm @ Metamodel(StringType) => mm.convert(Converter[String, Int](_.substring(1).toInt, _.toString))
    }

    testMetamodel.scalaType should equal(ArrayType(IntType))
    testMetamodel.converter.to(Array(1)) should be(Array(Some(1)))
    testMetamodel.converter.from(Array(Some(2))) should be(Array(2))

    testMetamodel.isInstanceOf[ConvertedMetamodel] should be(true)
    val cv = testMetamodel.asInstanceOf[ConvertedMetamodel]
    cv.visibleMetamodel.scalaType should be(ArrayType(OptionType(IntType)))
  }
}