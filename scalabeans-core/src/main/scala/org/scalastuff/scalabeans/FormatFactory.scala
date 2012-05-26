package org.scalastuff.scalabeans

import org.scalastuff.scalabeans.Preamble.scalaTypeOf
import org.scalastuff.scalabeans.types.ScalaType
import org.scalastuff.util.Format

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
  
  def withRewriteRules(scalaTypeRules: Rules[ScalaType]): This
  
  def formatFor[A <: AnyRef : Manifest](): F[A] = formatFor[A](scalaTypeOf[A])
  
  protected def formatFor[A <: AnyRef : Manifest](scalaType: ScalaType): F[A]
}
