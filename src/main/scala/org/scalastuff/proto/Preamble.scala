/**
 * Copyright (c) 2011 ScalaStuff.org (joint venture of Alexander Dvorkovyy and Ruud Diterwich)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package org.scalastuff.proto

import org.scalastuff.proto.value._
import org.scalastuff.scalabeans.Preamble._

object Preamble {
  def readerOf[B <: AnyRef](implicit mf: Manifest[B]): Reader[B] = {
    val scalaType = scalaTypeOf[B]
    ValueHandler(scalaType) match {
      case Some(beanValueHandler: BeanValueHandler) => new BeanReader[B]()
      case Some(_) => new WrappedReader[B]() 
      case None => throw new RuntimeException("Cannot create reader for type %s: this type or one of the type arguments is not supported".format(scalaType))
    }
  }
  
  
  def writerOf[B <: AnyRef](implicit mf: Manifest[B]): Writer[B] = {
    val scalaType = scalaTypeOf[B]
    ValueHandler(scalaType) match {
      case Some(beanValueHandler: BeanValueHandler) => new BeanWriter[B]()
      case Some(_) => new WrappedWriter[B]() 
      case None => throw new RuntimeException("Cannot create writer for type %s: this type or one of the type arguments is not supported".format(scalaType))
    }
  }

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