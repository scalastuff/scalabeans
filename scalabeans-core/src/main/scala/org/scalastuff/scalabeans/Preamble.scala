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

package org.scalastuff.scalabeans

import java.lang.reflect.Type

import org.scalastuff.scalabeans.types.ScalaType

object Preamble extends MetamodelRules with Metamodels {
  def metamodelOf[T : Manifest] = Metamodel[T]()

  def metamodelOf(scalaType: ScalaType) = Metamodel(scalaType)
  
  def descriptorOf[T <: AnyRef](implicit mf: Manifest[T]) = BeanDescriptor[T](mf)

  def descriptorOf(beanType: ScalaType) = BeanDescriptor(beanType)
  
  def descriptorOf(t: Type) = BeanDescriptor(scalaTypeOf(t))
  
  def scalaTypeOf[T](implicit mf: Manifest[T]) = ScalaType.scalaTypeOf(mf)

  def scalaTypeOf[T](t : Type) = ScalaType.scalaTypeOf(ManifestFactory.manifestOf(t))
  
  
}
