package org.scalastuff.scalabeans.format.json

import org.scalastuff.scalabeans._
import org.scalastuff.util.StringFormat
import org.scalastuff.util.Rules
import org.scalastuff.scalabeans.Preamble._
import com.fasterxml.jackson.core.JsonFactory

class JsonFormatFactory private (rules: Rules[Metamodel], jsonFactory: JsonFactory) extends FormatFactory {
  type This = JsonFormatFactory
  type F[A] = StringFormat[A]  

  def withRewriteRules(rules: Rules[Metamodel]) = new JsonFormatFactory(rules, jsonFactory)
  def withJsonFactory(jsonFactory: JsonFactory) = new JsonFormatFactory(rules, jsonFactory)

  protected def formatFor[T <: AnyRef: Manifest](metamodel: Metamodel): JsonFormat[T] = {
    val updatedMetamodel = metamodel rewrite rules
    new JsonFormat(jsonFactory, JsonHandler(metamodel))
  }
}

object JsonFormatFactory extends JsonFormatFactory(EmptyMetamodelRules, new JsonFactory())