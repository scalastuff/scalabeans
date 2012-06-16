package org.scalastuff.scalabeans

import org.scalastuff.scalabeans.types.ScalaType
import Preamble._
import org.scalastuff.util.Converter

/**
 * Provides interface for creation of Converter[A, Target] objects for specific ScalaType.
 * 
 * Each target type has it's own converter factory
 * to produce type-specific Converter[A, Target] objects.
 * 
 * @see org.scalastuff.util.Convertor 
 */
trait ConverterFactory {
  /**
   * Concrete type of the factory
   */
  type This <: ConverterFactory

  /**
   * Target type of produced converter
   */
  type Target

  /**
   * Concrete converter type
   */
  type C[A] <: Converter[A, Target]

  def withRewriteRules(scalaTypeRules: Rules[ScalaType]): This

  def convertorFor[A <: AnyRef: Manifest](): C[A] = convertorFor[A](scalaTypeOf[A])

  protected def convertorFor[A <: AnyRef: Manifest](scalaType: ScalaType): C[A]
}