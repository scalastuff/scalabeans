package org.scalastuff.scalabeans

import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.scalabeans.types.ScalaType
import org.scalastuff.util.Format
import org.scalastuff.util.Rules

/**
 * Provides interface for creation of Format[A] objects for specific ScalaType.
 * 
 * Each format (like protobuf, json etc) has it's own format factory
 * to produce type-specific Format[A] objects.
 * 
 * @see org.scalastuff.util.Format 
 */
trait FormatFactory {
  type This <: FormatFactory
  type F[A] <: Format[A] 
  
  def withRewriteRules(rules: Rules[Metamodel]): This
  
  def formatFor[A <: AnyRef : Manifest](): F[A] = formatFor[A](metamodelOf[A])
  
  protected def formatFor[A <: AnyRef : Manifest](metamodel: Metamodel): F[A]
}
