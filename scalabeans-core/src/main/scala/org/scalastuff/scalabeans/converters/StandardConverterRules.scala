package org.scalastuff.scalabeans.converters

import org.scalastuff.scalabeans.types._
import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.scalabeans._
import java.lang.{ Enum => JEnum }

object StandardConverterRules {

  val DateAsMilliseconds = metamodelRules {
    case mm @ MetaModel(DateType) => mm.addConverter(StandardConverters.DateAsMilliseconds)
  }
  
  val SqlDateAsString = metamodelRules {
    case mm @ MetaModel(SqlDateType) => mm.addConverter(StandardConverters.SqlDateAsString)
  }
  
  val SqlTimestampAsString = metamodelRules {
    case mm @ MetaModel(SqlTimestampType) => mm.addConverter(StandardConverters.SqlTimestampAsString)
  }
  
  val EnumAsName = metamodelRules {
    case mm @ MetaModel(EnumType(enum)) => 
      mm.addConverter(
          RuntimeConverter(
              mm.visibleMetaModel.scalaType,
              StringType,
              new StandardConverters.EnumAsName(enum)))
  }
  
  val JavaEnumAsName = metamodelRules {
    case mm @ MetaModel(JavaEnumType(values)) => 
      mm.addConverter(
          RuntimeConverter(
              mm.visibleMetaModel.scalaType,
              StringType,
              new StandardConverters.JavaEnumAsName(values)))
  }
}