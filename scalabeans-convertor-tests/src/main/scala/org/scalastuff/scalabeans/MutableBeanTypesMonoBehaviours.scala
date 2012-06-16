package org.scalastuff.scalabeans
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import java.util.UUID

trait MutableBeanTypesMonoBehaviours  extends MonoBehaviours with ShouldMatchers { self: FlatSpec =>

  import mutabletestbeans._

  def mutableBeanTypesMono() {
    it should "have inverse function for a mutable bean with properties of primitive types" in {
      checkInverse(createMono[PrimitiveTypesBean], List(new PrimitiveTypesBean, new PrimitiveTypesBean().set1))
    }

    it should "have inverse function for a mutable bean with properties of simple types" in {
      checkInverse(createMono[SimpleTypesBean], List(new SimpleTypesBean, new SimpleTypesBean().set1))
    }

    it should "have inverse function for a mutable bean with properties of Option types" in {
      checkInverse(createMono[OptionTestBean], List(new OptionTestBean, new OptionTestBean().set1))
    }

    it should "have inverse function for a mutable bean with properties of scalastuff Enum types" in {
      checkInverse(createMono[EnumTestBean], List(new EnumTestBean, new EnumTestBean().set1))
    }

    it should "have inverse function for a mutable bean with properties of java Enum types" in {
      checkInverse(createMono[JavaEnumTestBean], List(new JavaEnumTestBean, new JavaEnumTestBean().set1))
    }

    it should "have inverse function for a mutable bean with default property values" in {
      checkInverse(createMono[DefaultValuesTestBean], List(new DefaultValuesTestBean, new DefaultValuesTestBean().set1))
    }

    it should "have inverse function for a mutable bean with default property value generator" in {
      checkInverse(createMono[DefaultValueGeneratorTestBean],
        List(new DefaultValueGeneratorTestBean, new DefaultValueGeneratorTestBean().set1()))
    }
    
    it should "have inverse function for a mutable bean with properties of other bean types" in {
      checkInverse(createMono[CompositeTestBean], List(new CompositeTestBean, new CompositeTestBean().set1()))
    }
  }
}

package mutabletestbeans {
  class PrimitiveTypesBean {
    var bt: Byte = _
    var s: Short = _
    var i: Int = _
    var l: Long = _
    var bool: Boolean = _
    var f: Float = _
    var d: Double = _
    var c: Char = _

    def set1() = {
      bt = 1
      s = 2
      i = 3
      l = 4
      bool = true
      f = 5.0f
      d = 6.0
      c = 'A'
      this
    }

    override def equals(obj: Any) = obj match {
      case other: PrimitiveTypesBean =>
        bt == other.bt &&
          s == other.s &&
          i == other.i &&
          l == other.l &&
          bool == other.bool &&
          math.abs(f - other.f) < 0.1 &&
          math.abs(d - other.d) < 0.1 &&
          c == other.c
      case _ => false
    }
  }

  case class SimpleTypesBean(
    var s: String = "",
    var bd: BigDecimal = 0,
    var bi: BigInt = 0) {

    def set1() = {
      s = "whatever"
      bd = 1.0
      bi = 2
      this
    }
  }

  case class OptionTestBean(
    var p: Option[Int] = None,
    var r1: Option[String] = None,
    var r2: Option[String] = None) {

    def set1() = {
      p = Some(1)
      r1 = Some("whatever")
      r2 = Some("")
      this
    }
  }

  class Gender private ()
  object Gender extends Enum[Gender] {
    val Unknown = new Gender
    val M = new Gender
    val F = new Gender
  }

  case class EnumTestBean(var e: Gender = Gender.Unknown) {
    def set1() = {
      e = Gender.M
      this
    }
  }

  case class JavaEnumTestBean {
    import java.lang.annotation.RetentionPolicy

    var e: RetentionPolicy = RetentionPolicy.CLASS

    def set1() = {
      e = RetentionPolicy.RUNTIME
      this
    }
  }

  class DefaultValuesTestBean {
    var bt: Byte = 1
    var s: Short = 2
    var i: Int = 3
    var l: Long = 4
    var bool: Boolean = true
    var f: Float = 6.0f
    var d: Double = 7.0
    var c: Char = 'A'
    var str: String = "whatever"
    var bd: BigDecimal = 8.0
    var bi: BigInt = 9

    var timestamp: java.util.Date = new java.util.Date()
    var dateTime: java.sql.Timestamp = java.sql.Timestamp.valueOf("2011-01-30 10:30:59")
    var date: java.sql.Date = java.sql.Date.valueOf("2010-10-25")

    var op: Option[Long] = Some(10)
    var or1: Option[String] = Some("11")
    var or2: Option[String] = Some("12")

    def set1() = {
      bt = 0
      s = 0
      i = 0
      l = 0
      bool = false
      f = 0f
      d = 0.0
      c = 0
      str = ""
      bd = 0
      bi = 0

      timestamp = new java.util.Date(0)
      dateTime = new java.sql.Timestamp(0)
      date = new java.sql.Date(0)

      op = None
      or1 = None
      or2 = Some("")
      this
    }

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

  class DefaultValueGeneratorTestBean {
    var id: UUID = UUID.randomUUID
    
    def set1() = {
      id = new UUID(0, 0)
      this
    }
    
    override def equals(a: Any) = a match {
      case other : DefaultValueGeneratorTestBean => this.id == other.id
      case _ => false
    }
  }

  case class CompositeTestBean(
    var pbt: PrimitiveTypesBean = new PrimitiveTypesBean,
    var stp: SimpleTypesBean = new SimpleTypesBean,
    var opt: OptionTestBean = new OptionTestBean,
    var dv: DefaultValuesTestBean = new DefaultValuesTestBean,
    var dvg: DefaultValueGeneratorTestBean = new DefaultValueGeneratorTestBean,
    var enumt: EnumTestBean = new EnumTestBean) {

    def set1() = {
      pbt.set1()
      stp.set1()
      opt.set1()
      dv.set1()
      dvg.set1()
      enumt.set1()
      this
    }
  }
}