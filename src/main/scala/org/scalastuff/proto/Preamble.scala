package org.scalastuff.proto

import org.scalastuff.scalabeans.Preamble._

object Preamble {
  def readerOf[B <: AnyRef](implicit mf: Manifest[B]) = new BeanReader[B]
  def writerOf[B <: AnyRef](implicit mf: Manifest[B]) = new BeanWriter[B]

  /**
   * Schema for bean serialization/deserialization.
   *
   * Bean must be instantiatable via newInstance() without constructor parameters.
   * No immutable properties in constructor parameters are allowed. If this requirements
   * are not met, use BeanBuilderSchema instead.
   */
  def beanSchemaOf[B <: AnyRef](implicit mf: Manifest[B]) = BeanSchema.schemaOf[B]
  
  /**
   * Schema for bean builder serialization/deserialization.
   */
  def beanBuilderSchemaOf[B <: AnyRef](implicit mf: Manifest[B]) = BeanBuilderSchema(descriptorOf[B])
}