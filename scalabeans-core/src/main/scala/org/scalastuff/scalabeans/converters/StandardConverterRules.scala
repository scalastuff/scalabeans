package org.scalastuff.scalabeans.converters

import org.scalastuff.scalabeans.types._
import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.scalabeans._
import java.lang.{ Enum => JEnum }

object StandardConverterRules {

  val DateAsMilliseconds = metamodelRules {
    case mm @ Metamodel(DateType) => mm.convert(StandardConverters.DateAsMilliseconds)
  }
  
  val SqlDateAsString = metamodelRules {
    case mm @ Metamodel(SqlDateType) => mm.convert(StandardConverters.SqlDateAsString)
  }
  
  val SqlTimestampAsString = metamodelRules {
    case mm @ Metamodel(SqlTimestampType) => mm.convert(StandardConverters.SqlTimestampAsString)
  }
  
  val EnumAsName = metamodelRules {
    case mm @ Metamodel(EnumType(enum)) => 
      mm.convert(
          RuntimeConverter(
              mm.visibleMetamodel.scalaType,
              StringType,
              new StandardConverters.EnumAsName(enum)))
  }
  
  val JavaEnumAsName = metamodelRules {
    case mm @ Metamodel(JavaEnumType(values)) => 
      mm.convert(
          RuntimeConverter(
              mm.visibleMetamodel.scalaType,
              StringType,
              new StandardConverters.JavaEnumAsName(values)))
  }
}