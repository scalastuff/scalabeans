package org.scalastuff.scalabeans.format.json

import org.scalastuff.scalabeans._
import org.scalastuff.util.Rules
import org.scalastuff.scalabeans.Preamble._
import com.fasterxml.jackson.core.JsonFactory
import org.scalastuff.scalabeans.format.FormatFactory
import org.scalastuff.scalabeans.format.StringFormat
import org.scalastuff.scalabeans.converters.StandardConverterRules

class JsonFormatFactory private (rules: Rules[Metamodel], jsonFactory: JsonFactory) extends FormatFactory {
  type This = JsonFormatFactory
  type F[A] = JsonFormat[A]

  def withRewriteRules(_rules: Rules[Metamodel]) = new JsonFormatFactory(_rules andThen rules, jsonFactory)
  def withJsonFactory(jsonFactory: JsonFactory) = new JsonFormatFactory(rules, jsonFactory)

  def formatFor(metamodel: Metamodel): JsonFormat[Any] = {
    val updatedMetamodel = metamodel rewrite rules
    new JsonFormat(jsonFactory, JsonHandler(updatedMetamodel))
  }
}

import StandardConverterRules._

object JsonFormatFactory extends {  
  val defaultRewriteRules = 
    DateAsMilliseconds compose 
    SqlDateAsString compose
    SqlTimestampAsString compose
    EnumAsName compose
    JavaEnumAsName
} with JsonFormatFactory(defaultRewriteRules, new JsonFactory())