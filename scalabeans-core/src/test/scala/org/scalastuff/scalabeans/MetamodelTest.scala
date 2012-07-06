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
class MetaModelTest extends FlatSpec with ShouldMatchers {
  "Converted ValueMetaModel" should "contain correct ScalaTypes and conversion" in {
    val intMetaModel = metaModelOf[Int] rewrite metamodelRules {
      case mm @ MetaModel(IntType) => mm.addConverter(Converter[Int, String](_.toString, _.toInt))
    }
    intMetaModel.scalaType should equal(IntType)
    intMetaModel.converter.to(1) should be("1")
    intMetaModel.converter.from("2") should be(2)

    intMetaModel.isInstanceOf[ConvertedMetaModel] should be(true)
    val cv = intMetaModel.asInstanceOf[ConvertedMetaModel]
    cv.visibleMetaModel.scalaType should be(StringType)
  }

  "Converted ArrayMetaModel" should "contain correct ScalaTypes and conversion" in {
    val testMetaModel = metaModelOf[Array[Int]] rewrite metamodelRules {
      case mm @ MetaModel(IntType) => mm.addConverter(Converter[Int, String](_.toString, _.toInt))
    }
    testMetaModel.scalaType should equal(ArrayType(IntType))
    testMetaModel.converter.to(Array(1)) should be(Array("1"))
    testMetaModel.converter.from(Array("2")) should be(Array(2))

    testMetaModel.isInstanceOf[ConvertedMetaModel] should be(true)
    val cv = testMetaModel.asInstanceOf[ConvertedMetaModel]
    cv.visibleMetaModel.scalaType should be(ArrayType(StringType))
  }

  "ArrayMetaModel, converted twice" should "contain correct ScalaTypes and conversion" in {
    val testMetaModel = metaModelOf[Array[Int]] rewrite metamodelRules {
      case mm @ MetaModel(IntType) => mm.addConverter(Converter[Int, String](_.toString, _.toInt))
    } rewrite metamodelRules {
      case mm @ MetaModel(StringType) => mm.addConverter(Converter[String, Int](_.toInt, _.toString))
    }

    testMetaModel.scalaType should equal(ArrayType(IntType))
    testMetaModel.converter.to(Array(1)) should be(Array(1))
    testMetaModel.converter.from(Array(2)) should be(Array(2))

    testMetaModel.isInstanceOf[ConvertedMetaModel] should be(true)
    val cv = testMetaModel.asInstanceOf[ConvertedMetaModel]
    cv.visibleMetaModel.scalaType should be(ArrayType(IntType))
  }

  "ValueMetaModel converted to ContainerMetaModel" should "be rewritable" in {
    val testMetaModel = metaModelOf[Int] rewrite metamodelRules {
      case mm @ MetaModel(IntType) => mm.addConverter(Converter[Int, Option[String]]({ i => Some("x" + i.toString) }, _.get.substring(1).toInt))
    } rewrite metamodelRules {
      case mm @ MetaModel(StringType) => mm.addConverter(Converter[String, Int](_.substring(1).toInt, "y" + _.toString))
    }

    testMetaModel.scalaType should equal(IntType)
    testMetaModel.converter.to(1) should be(Some(1))
    testMetaModel.converter.from(Some(2)) should be(2)

    testMetaModel.isInstanceOf[ConvertedMetaModel] should be(true)
    val cv = testMetaModel.asInstanceOf[ConvertedMetaModel]
    cv.visibleMetaModel.scalaType should be(OptionType(IntType))
    cv.visibleMetaModel.isInstanceOf[ContainerMetaModel] should be(true)
  }

  "ValueMetaModel inside container converted to ContainerMetaModel" should "be rewritable" in {
    val testMetaModel = metaModelOf[Array[Int]] rewrite metamodelRules {
      case mm @ MetaModel(IntType) => mm.addConverter(Converter[Int, Option[String]]({ i => Some("x" + i.toString) }, _.get.toInt))
    } rewrite metamodelRules {
      case mm @ MetaModel(StringType) => mm.addConverter(Converter[String, Int](_.substring(1).toInt, _.toString))
    }

    testMetaModel.scalaType should equal(ArrayType(IntType))
    testMetaModel.converter.to(Array(1)) should be(Array(Some(1)))
    testMetaModel.converter.from(Array(Some(2))) should be(Array(2))

    testMetaModel.isInstanceOf[ConvertedMetaModel] should be(true)
    val cv = testMetaModel.asInstanceOf[ConvertedMetaModel]
    cv.visibleMetaModel.scalaType should be(ArrayType(OptionType(IntType)))
  }
}