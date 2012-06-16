package org.scalastuff.scalabeans

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalastuff.scalabeans.Enum
import java.util.UUID
import scala.util.Random

trait ImmutableBeanTypesMonoBehaviours extends MonoBehaviours with ShouldMatchers { self: FlatSpec =>

  import immutabletestbeans._

  def immutableBeanTypesMono() {
    it should "have inverse function for a bean without properties" in {
      checkInverse(createMono[EmptyTestBean], List(new EmptyTestBean))
    }

    it should "have inverse function for a bean with properties of primitive types" in {
      checkInverse(createMono[PrimitiveTypesBean], List(new PrimitiveTypesBean, new PrimitiveTypesBean().set1))
    }

    it should "have inverse function for a bean with properties of simple types" in {
      checkInverse(createMono[SimpleTypesBean], List(new SimpleTypesBean, new SimpleTypesBean().set1))
    }

    it should "have inverse function for a bean with properties of Option types" in {
      checkInverse(createMono[OptionTestBean], List(new OptionTestBean, new OptionTestBean().set1))
    }

    it should "have inverse function for a bean with properties of scalastuff Enum types" in {
      checkInverse(createMono[EnumTestBean], List(new EnumTestBean, new EnumTestBean().set1))
    }

    it should "have inverse function for a bean with properties of java Enum types" in {
      checkInverse(createMono[JavaEnumTestBean], List(new JavaEnumTestBean, new JavaEnumTestBean().set1))
    }

    it should "have inverse function for a bean with default property values" in {
      checkInverse(createMono[DefaultValuesTestBean], List(new DefaultValuesTestBean, new DefaultValuesTestBean().set1))
    }

    it should "have inverse function for a bean with default property value generator" in {
      checkInverse(createMono[DefaultValueGeneratorTestBean],
        List(new DefaultValueGeneratorTestBean, new DefaultValueGeneratorTestBean().set1()))
    }
    
    it should "have inverse function for a bean with properties of other bean types" in {
      checkInverse(createMono[CompositeTestBean], List(new CompositeTestBean, new CompositeTestBean().set1()))
    }
  }
}

package immutabletestbeans {
  case class EmptyTestBean

  case class PrimitiveTypesBean(
    bt: Byte = 0,
    s: Short = 0,
    i: Int = 0,
    l: Long = 0,
    bool: Boolean = false,
    f: Float = 0.0f,
    d: Double = 0.0,
    c: Char = ' ') {

    def set1() = PrimitiveTypesBean(1, 2, 3, 4, true, 5.0f, 6.0, 'A')

    override def equals(obj: Any) = obj match {
      case other: PrimitiveTypesBean =>
        bt == other.bt &&
          s == other.s &&
          i == other.i &&
          l == other.l &&
          bool == other.bool &&
          Math.abs(f - other.f) < 0.1 &&
          Math.abs(d - other.d) < 0.1 &&
          c == other.c
      case _ => false
    }
  }

  case class SimpleTypesBean(
    s: String = "",
    bd: BigDecimal = 0,
    bi: BigInt = 0) {

    def set1() = SimpleTypesBean("whatever", 1.0, 2)
  }

  case class OptionTestBean(
    val p: Option[Int] = None,
    val r1: Option[String] = None,
    val r2: Option[String] = None) {

    def set1() = OptionTestBean(Some(1), Some("whatever"), Some(""))
  }

  class Gender private ()
  object Gender extends Enum[Gender] {
    val Unknown = new Gender
    val M = new Gender
    val F = new Gender
  }

  case class EnumTestBean(val e: Gender = Gender.Unknown) {
    def set1() = EnumTestBean(Gender.M)
  }

  import java.lang.annotation.RetentionPolicy
  case class JavaEnumTestBean(e: RetentionPolicy = RetentionPolicy.CLASS) {    
    def set1() = JavaEnumTestBean(RetentionPolicy.RUNTIME)
  }

  case class DefaultValuesTestBean(
    bt: Byte = 1,
    s: Short = 2,
    i: Int = 3,
    l: Long = 4,
    bool: Boolean = true,
    f: Float = 6.0f,
    d: Double = 7.0,
    c: Char = 'A',
    str: String = "whatever",
    bd: BigDecimal = 8.0,
    bi: BigInt = 9,

    timestamp: java.util.Date = new java.util.Date(),
    dateTime: java.sql.Timestamp = java.sql.Timestamp.valueOf("2011-01-30 10:30:59"),
    date: java.sql.Date = java.sql.Date.valueOf("2010-10-25"),

    op: Option[Long] = Some(10),
    or1: Option[String] = Some("11"),
    or2: Option[String] = Some("12")) {

    def set1() = DefaultValuesTestBean(0, 0, 0, 0, false, 0f, 0.0, 0, "", 0, 0, 
        new java.util.Date(0), new java.sql.Timestamp(0), new java.sql.Date(0), None, None, Some(""))

    override def equals(obj: Any) = obj match {
      case other: DefaultValuesTestBean =>
        bt == other.bt &&
          s == other.s &&
          i == other.i &&
          l == other.l &&
          bool == other.bool &&
          Math.abs(f - other.f) < 0.1 &&
          Math.abs(d - other.d) < 0.1 &&
          c == other.c &&
          str == other.str &&
          bd == other.bd &&
          bi == other.bi &&
          timestamp == other.timestamp &&
          dateTime == other.dateTime &&
          date == other.date &&
          op == other.op &&
          or1 == other.or1 &&
          or2 == other.or2
      case _ => false
    }
  }

  
  case class DefaultValueGeneratorTestBean(id: Long = Random.nextLong) {
    def set1() = DefaultValueGeneratorTestBean(0)
  }

  case class CompositeTestBean(
    e: EmptyTestBean = EmptyTestBean(),
    pbt: PrimitiveTypesBean = new PrimitiveTypesBean,
    stp: SimpleTypesBean = new SimpleTypesBean,
    opt: OptionTestBean = new OptionTestBean,
    dv: DefaultValuesTestBean = new DefaultValuesTestBean,
    dvg: DefaultValueGeneratorTestBean = new DefaultValueGeneratorTestBean,
    enumt: EnumTestBean = new EnumTestBean) {

    def set1() = CompositeTestBean(
        EmptyTestBean(), 
        PrimitiveTypesBean().set1(),
        SimpleTypesBean().set1(),
        OptionTestBean().set1(),
        DefaultValuesTestBean().set1(),
        DefaultValueGeneratorTestBean().set1(),
        EnumTestBean().set1())
  }
}