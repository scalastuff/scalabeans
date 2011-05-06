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

import org.scalastuff.scalabeans.types.ScalaType

/**
 * Contains bean information.
 */
trait BeanDescriptor {
  
  /**
   * @return bean class name
   */
  def name = beanType.erasure.getName()
  
  /**
   * Encapsulates bean class
   * 
   * @see [[org.scalastuff.scalabeans.types.ScalaType]]
   */
  def beanType: ScalaType

  /**
   * Bean property descriptors
   * 
   * @see [[org.scalastuff.scalabeans.ImmutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.MutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.ConstructorParameter]]
   */
  def properties: Seq[PropertyDescriptor]

  /**
   * Bean property descriptor lookup
   * 
   * @see [[org.scalastuff.scalabeans.ImmutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.MutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.ConstructorParameter]]
   */
  def property(name: String): Option[PropertyDescriptor] = properties find (_.name == name)

  /**
   * Convenience method for getting property descriptor by property name.
   * 
   * @see [[org.scalastuff.scalabeans.ImmutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.MutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.ConstructorParameter]]
   * 
   * @throws IllegalArgumentException if property not found
   */
  def apply(name: String) = property(name) getOrElse 
  	(throw new IllegalArgumentException("Property %s.%s not found".format(this.name, name)))

  private[this] lazy val builderFactory = new BeanBuilderFactory(this, properties.toList)

  /**
   * Creates new BeanBuilder instance.
   * 
   * @see [[org.scalastuff.scalabeans.BeanBuilder]]
   */
  def newBuilder(): BeanBuilder = builderFactory.newBuilder

  /**
   * Checks if new instance can be created without BeanBuilder.
   * 
   * Returns true if one of the constructor parameters:
   *  * is immutable: only way to set it is via constructor
   *  * doesnt have default value
   */
  def needsBeanBuilder: Boolean = {
    properties exists { prop =>
      prop match {
        case cp:ConstructorParameter =>
          prop match {
            case _:ImmutablePropertyDescriptor => true
            case _:MutablePropertyDescriptor => cp.defaultValue.isEmpty
          }
        case _ => false
      }
    }
  }
  
  /**
   * @return bean class companion object (if any defined)
   */
  lazy val companion : Option[AnyRef] = try {
    val cc = Class.forName(beanType.erasure.getName + "$")
    Some(cc.getField("MODULE$").get(cc))
  } catch {
    case e =>
      e.printStackTrace
      None
  }

  // TODO: views

  /**
   * Creates new bean instance.
   * 
   * Constructor parameters must be provided in the same order specified in default constructor.
   * Default values are supported.
   * 
   * @see [[org.scalastuff.scalabeans.BeanBuilder]]
   */
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
   * Convenience method to get property value.
   * 
   * @param obj bean instance which property must be looked up
   * @param propertyName property name
   * 
   * @see [[org.scalastuff.scalabeans.PropertyDescriptor]] 
   */
  def get(obj: AnyRef, propertyName: String) = apply(propertyName).get[Any](obj)
  
  /**
   * Convenience method to set property value.
   *    
   * @param obj bean instance which property must be looked up
   * @param propertyName property name
   * @param value new property value
   * 
   * @throws IllegalArgumentException if property is immutable
   * 
   * @see [[org.scalastuff.scalabeans.MutablePropertyDescriptor]]
   */
  def set(obj: AnyRef, propertyName: String, value: Any) {
    apply(propertyName) match {
      case mutable: MutablePropertyDescriptor => mutable.set(obj, value)
      case _ => throw new IllegalArgumentException("Cannot set value: property %s.%s is immutable".format(this.name, propertyName))
    }
  }

  /**
   * Top-level class in super-class hierarchy that is not
   * java.lang.Object
   */
  def topLevelClass: Class[_]

  override def toString = beanType.toString
}