package org.scalastuff.scalabeans.converters

import org.scalastuff.scalabeans.Enum
import java.lang.{ Enum => JEnum }

object StandardConverters {

  val DateAsMilliseconds = Converter[java.util.Date, Long](_.getTime(), new java.util.Date(_))
  val SqlDateAsString = Converter[java.sql.Date, String](_.toString(), java.sql.Date.valueOf(_))
  val SqlTimestampAsString = Converter[java.sql.Timestamp, String](_.toString(), java.sql.Timestamp.valueOf(_))

  class EnumAsName[A <: AnyRef](enum: Enum[A]) extends Converter[A, String] {
    def to(u: A) = enum.nameOf(u)

    def from(v: String) = enum.valueOf(v) getOrElse {
      throw new IllegalArgumentException("One of %s expected, found \"%s\"".
        format(enum.names.mkString("[", ",", "]"), v))
    }
  }

  class JavaEnumAsName[A <: JEnum[_]](values: Iterable[A]) extends Converter[A, String] {
    def to(u: A) = u.name

    def from(v: String) = values.find(_.name == v) getOrElse {
      throw new IllegalArgumentException("One of %s expected, found \"%s\"".
        format(values.map(_.name).mkString("[", ",", "]"), v))
    }
  }
}