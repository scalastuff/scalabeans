package org.scalastuff.scalabeans

import org.scalastuff.scalabeans.types.ScalaType
import Preamble._
import org.scalastuff.util.Converter
import org.scalastuff.util.Rules

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

  def withRewriteRules(rules: Rules[Metamodel]): This

  def converterFor[A <: AnyRef: Manifest](): C[A] = converterFor[A](scalaTypeOf[A])

  protected def converterFor[A <: AnyRef: Manifest](scalaType: ScalaType): C[A]
}