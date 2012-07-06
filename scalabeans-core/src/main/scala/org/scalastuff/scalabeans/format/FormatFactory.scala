package org.scalastuff.scalabeans.format

import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.scalabeans.types.ScalaType
import org.scalastuff.util.Rules
import org.scalastuff.scalabeans.MetaModel

/**
 * Provides interface for creation of Format[A] objects of specific type.
 * 
 * Each format (like protobuf, json etc) has it's own format factory
 * to produce type-specific Format[A] objects.
 * 
 * @see org.scalastuff.util.Format 
 */
trait FormatFactory {
  type This <: FormatFactory
  type F[A] <: Format[A] 
  
  def addRewriteRules(rules: Rules[MetaModel]): This
  
  def formatFor[A <: AnyRef : Manifest](): F[A] = formatFor(metaModelOf[A]).asInstanceOf[F[A]]
  
  def formatFor(metaModel: MetaModel): F[Any]
}
