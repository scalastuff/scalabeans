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

import java.lang.reflect.Constructor
import scala.collection.mutable.ArrayBuffer


/**
 * Builds new bean instance from available property values.
 * 
 * Figures out automatically constructor parameters, default values, mutable properties.
 * 
 * New BeanBuilder instance is provided by BeanDescriptor:
 * {{{
 * val builder = descriptor.newBuilder
 * }}}
 *
 * provide than all known property values:
 * {{{
 * builder.set(descriptor("property1"), value1)
 * builder.set(descriptor("property2"), value2)
 * ...
 * }}}
 * 
 * and call result() to get new bean instance:
 * {{{
 * val newInstance = builder.result()
 * }}}
 * 
 */
abstract class BeanBuilder {
  def beanDescriptor: BeanDescriptor
  def constructor: Constructor[AnyRef]

  def constructorParams: List[ConstructorParameter]
  def mutableProperties: List[MutablePropertyDescriptor]

  def lastConstructorParamIndex: Int
  def lastMutablePropertyIndex: Int

  private[this] val constructorParamValues = Array.ofDim[AnyRef](lastConstructorParamIndex + 1)
  protected val unsetConstructorParams: Array[Boolean]

  private[this] val mutablePropertyValues = Array.ofDim[Any](lastMutablePropertyIndex + 1)
  protected val unsetMutableProperties: Array[Boolean]

  def unsetProperties(): List[DeserializablePropertyDescriptor] = {
    (constructorParams filter { prop => unsetConstructorParams.contains(prop.index) }) :::
      (mutableProperties filter { prop => unsetMutableProperties.contains(prop.index) })
  }

  /**
   * Returns `true` if property value was set by `set(..)` method. 
   */
  def isSet(property: PropertyDescriptor): Boolean = property match {
    case cp: ConstructorParameter => !unsetConstructorParams(cp.index)
    case mp: MutablePropertyDescriptor => !unsetMutableProperties(mp.index)
    case _ => false // cannot set immutable property
  }

  /**
   * Receives value used to initialize given property of new bean instance.
   * 
   * Only constructor parameters and mutable properties are supported.
   * Method throws IllegalArgumentException on attempt to set immutable property value.
   */
  def set(property: PropertyDescriptor, value: Any): Unit = property match {
    case cp: ConstructorParameter =>
      constructorParamValues(cp.index) = value.asInstanceOf[AnyRef]
      unsetConstructorParams(cp.index) = false
    case mp: MutablePropertyDescriptor =>
      mutablePropertyValues(mp.index) = value
      unsetMutableProperties(mp.index) = false
    case _ => throw new IllegalArgumentException("Cannot set property value: only constructor parameters and mutable properties are accepted")
  }

  def get[A](property: PropertyDescriptor): A = property match {
    case cp: ConstructorParameter => constructorParamValues(cp.index).asInstanceOf[A]
    case mp: MutablePropertyDescriptor => mutablePropertyValues(mp.index).asInstanceOf[A]
    case _ => error("Cannot get property value: only constructor parameters and mutable properties are accepted")
  }

  /**
   * Constructs new bean instance. 
   * 
   * Uses values provided via `set(..)` method. Constructor parameters and mutable properties are supported.
   */
  def result() = {
    def reprortMissingConstructorParameters(): Nothing = {
      val missingParameterNames =
        for {
          index <- 0 to unsetConstructorParams.length - 1 if unsetConstructorParams(index)
          param <- constructorParams find (_.index == index)
          if param.defaultValue.isEmpty
        } yield param.name

      error("Cannot instantiate object of class %s : missing constructor parameters %s".
        format(beanDescriptor.toString, missingParameterNames mkString ", "))
    }

    var index = 0
    while (index < unsetConstructorParams.length) {
      if (unsetConstructorParams(index)) {
        val constructorParam = constructorParams find (_.index == index) get
        val defaultValue = constructorParam.defaultValue getOrElse reprortMissingConstructorParameters()
        constructorParamValues(index) = defaultValue().asInstanceOf[AnyRef]

      }
      index += 1
    }

    val instance = constructor.newInstance(constructorParamValues: _*)

    mutableProperties withFilter { prop => !unsetMutableProperties(prop.index) } foreach { prop =>
      prop.set(instance, mutablePropertyValues(prop.index))
    }

    instance
  }
}

class BeanBuilderFactory(val beanDescriptor: BeanDescriptor, properties: List[PropertyDescriptor]) {
  val constructor: Constructor[AnyRef] = {
    val c = beanDescriptor.beanType.erasure
    require(c.getConstructors().size > 0,
      "Cannot create BeanBuilderFactory for %s: it has no constructors (either abstract class or interface)".
        format(beanDescriptor.toString))

    c.getConstructors()(0).asInstanceOf[Constructor[AnyRef]]
  }

  constructor.setAccessible(true)

  /**
   * All constructor properties of BeanDescriptor sorted by index, not limited by view
   */
  val constructorParams = {
    val constructorParameters = beanDescriptor.properties collect {
      case p: ConstructorParameter => p
    }

    if (!constructorParameters.isEmpty) {
      val lastConstructorParameterIndex = constructorParameters.foldLeft(-1)(_ max _.index)
      require(lastConstructorParameterIndex + 1 == constructorParameters.size,
        "Constructor parameter list for %s is incomplete or inconsistent: found parameter with index %d, found constructor parameters %d".
          format(beanDescriptor.toString, lastConstructorParameterIndex, constructorParameters.size))
    }

    require(constructorParameters.size == constructor.getParameterTypes.size,
      "Constructor parameter list for %s is incomplete: discovered %d parameters of %d. Declare missing parameters with val or var.".
        format(beanDescriptor.toString, constructorParameters.size, constructor.getParameterTypes.size))

    constructorParameters.sortWith(_.index < _.index).toList
  }

  private val unsetConstructorParams = ArrayBuffer.fill[Boolean](constructorParams.size)(true) toArray

  val mutableProperties = properties collect {
    case mp: MutablePropertyDescriptor if !mp.isInstanceOf[ConstructorParameter] => mp
  }

  val lastMutablePropertyIndex = mutableProperties.foldLeft(-1)(_ max _.index)
  private val unsetMutableProperties = Array.ofDim[Boolean](lastMutablePropertyIndex + 1)
  for (prop <- mutableProperties)
    unsetMutableProperties(prop.index) = true

  def newBuilder = new BeanBuilder {
    val beanDescriptor = BeanBuilderFactory.this.beanDescriptor
    def constructor = BeanBuilderFactory.this.constructor

    def constructorParams = BeanBuilderFactory.this.constructorParams
    def lastConstructorParamIndex = BeanBuilderFactory.this.constructorParams.size - 1
    protected val unsetConstructorParams = BeanBuilderFactory.this.unsetConstructorParams.clone()

    def mutableProperties = BeanBuilderFactory.this.mutableProperties
    def lastMutablePropertyIndex = BeanBuilderFactory.this.lastMutablePropertyIndex
    protected val unsetMutableProperties = BeanBuilderFactory.this.unsetMutableProperties.clone()
  }
}