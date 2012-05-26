package org.scalastuff.scalabeans

import org.scalastuff.scalabeans.types.ScalaType
import Preamble._
import org.scalastuff.util.Convertor

/**
 * Provides interface for creation of Convertor[A, Target] objects for specific ScalaType.
 * 
 * Each target type has it's own convertor factory
 * to produce type-specific Convertor[A, Target] objects.
 * 
 * @see org.scalastuff.util.Convertor 
 */
trait ConvertorFactory {
  /**
   * Concrete type of the factory
   */
  type This <: ConvertorFactory

  /**
   * Target type of produced convertor
   */
  type Target

  /**
   * Concrete convertor type
   */
  type C[A] <: Convertor[A, Target]

  def withRewriteRules(scalaTypeRules: Rules[ScalaType]): This

  def convertorFor[A <: AnyRef: Manifest](): C[A] = convertorFor[A](scalaTypeOf[A])

  protected def convertorFor[A <: AnyRef: Manifest](scalaType: ScalaType): C[A]
}