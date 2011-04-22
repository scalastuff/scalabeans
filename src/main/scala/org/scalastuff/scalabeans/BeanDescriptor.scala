/*
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

import org.scalastuff.scalabeans.types.ScalaType

trait BeanDescriptor {
  def beanType: ScalaType

  def properties: Seq[PropertyDescriptor]

  def property(name: String) = properties find (_.name == name)

  def apply(name: String) = property(name).get

  private[this] lazy val builderFactory = new BeanBuilderFactory(this, properties.toList)

  def newBuilder() = builderFactory.newBuilder

  def hasImmutableConstructorParameters: Boolean = {
    properties exists {
      prop => prop.isInstanceOf[ImmutablePropertyDescriptor] && prop.isInstanceOf[ConstructorParameter]
    }
  }
  
  lazy val companion : Option[AnyRef] = try {
    Some(beanType.erasure.getField("MODULE$").get(beanType.erasure))
  } catch {
    case e => None
  }

  // TODO: views

  def newInstance(args: AnyRef*): AnyRef = {
    if (args.size < builderFactory.constructorParams.size) {

      val builder = newBuilder()

      for (prop <- builder.constructorParams.iterator take args.size)
        builder.set(prop, args(prop.index))

      builder.result()

    } else {
      builderFactory.constructor.newInstance(args: _*)
    }
  }

  /**
   * Top-level class in super-class hierarchy that is not
   * java.lang.Object
   */
  def topLevelClass: Class[_]

  override def toString = beanType.toString
}