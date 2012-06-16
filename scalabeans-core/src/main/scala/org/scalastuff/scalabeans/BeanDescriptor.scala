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

import org.scalastuff.scalabeans.types.{ ScalaType, BeanType }

/**
 * Contains bean information.
 */
trait BeanDescriptor {

  /**
   * @return bean class name
   */
  def name = manifest.erasure.getName()

  /**
   * Encapsulates bean class
   *
   * @see [[org.scalastuff.scalabeans.types.ScalaType]]
   */
  def manifest: Manifest[_]

  /**
   * Bean property descriptors
   *
   * @see [[org.scalastuff.scalabeans.ImmutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.MutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.ConstructorParameter]]
   */
  def properties: Seq[PropertyDescriptor]
  
  /**
   * Exclude properties with given names.
   * 
   * Constructor parameters cannot be excluded.
   */
  def exclude(propertyNames: String*) = {
    val ctorParams = propertyNames filter { propertyName =>
       this(propertyName).isInstanceOf[ConstructorParameter]
    }
    require(ctorParams.isEmpty, "Cannot exclude properties: %s are constructor parameters".format(ctorParams))
    
    updateProperties(properties filter {property => !propertyNames.contains(property.name)})
  }
  
  /**
   * Creates a copy of this BeanDescriptor with new property added
   */
  def addProperty[B <: AnyRef, P: Manifest](name: String, getter: B => P, setter: Option[(B, P) => Unit]): BeanDescriptor = {
    val newTag = (properties.view map (_.tag) max) + 1
    addProperty(name, getter, setter, newTag)
  }
    
  /**
   * Creates a copy of this BeanDescriptor with new property added
   */
  def addProperty[B <: AnyRef, P: Manifest](name: String, getter: B => P, setter: Option[(B, P) => Unit], newTag: Int): BeanDescriptor = {
    val newPropertyModel = 
      new PropertyDescriptor.Model(
        manifest, 
        name, 
        Preamble.scalaTypeOf[P], 
        newTag, 
        getter.asInstanceOf[AnyRef => Any], 
        setter.asInstanceOf[Option[(AnyRef, Any) => Unit]])
    
    updateProperties(properties :+ PropertyDescriptor(newPropertyModel, 0, -1, None))
  }

  private[scalabeans] def updateProperties(newProperties: Seq[PropertyDescriptor]) = {
    def duplicates[A](coll: Seq[A]) = coll groupBy {x => x} filter {case (_, lst) => lst.size > 1} keys
    
    val nameDuplicates = duplicates(newProperties map (_.name))
    require(nameDuplicates.isEmpty, 
        "Cannot update properties of bean %s, following property names are not unique: %s".
        format(this, nameDuplicates mkString ","))
    
    val tagDuplicates = duplicates(newProperties map (_.tag))
    require(tagDuplicates.isEmpty, 
        "Cannot update properties of bean %s, following property tags are not unique: %s".
        format(this, tagDuplicates mkString ","))
    
    new BeanDescriptor {
      val manifest = BeanDescriptor.this.manifest
      val constructor = BeanDescriptor.this.constructor

      val properties = { // reindex mutable properties
        var mutablePropertyIndex = 0

        for (property <- newProperties) yield {
          if (property.isInstanceOf[MutablePropertyDescriptor] && !property.isInstanceOf[ConstructorParameter]) {
            val res = PropertyDescriptor(property.model, mutablePropertyIndex, -1, None)
            mutablePropertyIndex += 1
            res
          } else {
            property
          }
        }
      }
    }
  }

  @deprecated(message = "Use propertyOption method", since = "0.3")
  def property(name: String) = propertyOption(name)
  
  /**
   * Bean property descriptor lookup
   *
   * @see [[org.scalastuff.scalabeans.ImmutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.MutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.ConstructorParameter]]
   */
  def propertyOption(name: String): Option[PropertyDescriptor] = properties find (_.name == name)

  /**
   * Convenience method for getting property descriptor by property name.
   *
   * @see [[org.scalastuff.scalabeans.ImmutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.MutablePropertyDescriptor]]
   * @see [[org.scalastuff.scalabeans.ConstructorParameter]]
   *
   * @throws IllegalArgumentException if property not found
   */
  def apply(name: String) = propertyOption(name) getOrElse
    (throw new IllegalArgumentException("Property %s.%s not found".format(this.name, name)))

  def constructor: Option[BeanConstructor]

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
        case cp: ConstructorParameter =>
          prop match {
            case _: ImmutablePropertyDescriptor => true
            case _: MutablePropertyDescriptor => cp.defaultValue.isEmpty
          }
        case _ => false
      }
    }
  }

  /**
   * @return bean class companion object (if any defined)
   */
  lazy val companion: Option[AnyRef] = try {
    val cc = Class.forName(manifest.erasure.getName + "$")
    Some(cc.getField("MODULE$").get(cc))
  } catch {
    case e =>
      e.printStackTrace
      None
  }

  /**
   * Creates new bean instance.
   *
   * Constructor parameters must be provided in the same order specified in default constructor.
   * Default values are supported.
   *
   * @see [[org.scalastuff.scalabeans.BeanBuilder]]
   */
  def newInstance(args: Any*): AnyRef = {
    require(args.size <= builderFactory.constructorParams.size,
      "Cannot create new instance of bean %s: too many arguments provided. Provided %d, expected %d.".
        format(manifest, args.size, builderFactory.constructorParams.size))

    val builder = newBuilder()

    for (prop <- builder.constructorParams.iterator take args.size)
      builder.set(prop, args(prop.index))

    builder.result()
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
   * Top-level class in super-class hierarchy that is not java.lang.Object
   */
  lazy val topLevelClass: Class[_] = {
    def getTopLevelClass(c: Class[_]): Class[_] = c.getSuperclass match {
      case null => c
      case superClass if superClass == classOf[java.lang.Object] => c
      case superClass => getTopLevelClass(superClass)
    }

    getTopLevelClass(manifest.erasure)
  }
  
  /**
   * Creates a copy of this BeanDescriptor without constructor.
   */
  def withoutConstructor() = BeanDescriptor(manifest, properties map (_.model), None)

  /**
   * Creates a copy of this BeanDescriptor using given constructor.
   */
  def withConstructor(ctor: () => AnyRef): BeanDescriptor = {    
    BeanDescriptor(
        manifest,
        properties.view.map(_.resetValueConvertor()).map(_.model),
        Some(ConstructorModel({ args: Array[AnyRef] => ctor() }, Seq.empty))
        )
  }
    
  /**
   * Creates a copy of this BeanDescriptor using given constructor.
   */
  def withConstructor[P1](ctor: P1 => AnyRef, arg1: Pair[String, Option[() => P1]]): BeanDescriptor = {
    BeanDescriptor(
        manifest,
        properties.view.map(_.resetValueConvertor()).map(_.model),
        Some(ConstructorModel({ args: Array[AnyRef] => ctor(args(0).asInstanceOf[P1]) }, Seq(arg1)))
        )
  }
  
  /**
   * Creates a copy of this BeanDescriptor using given constructor.
   */
  def withConstructor[P1, P2](ctor: (P1, P2) => AnyRef, arg1: Pair[String, Option[() => P1]], arg2: Pair[String, Option[() => P2]]): BeanDescriptor = {
    BeanDescriptor(
        manifest,
        properties.view.map(_.resetValueConvertor()).map(_.model),
        Some(ConstructorModel({ args: Array[AnyRef] => 
          ctor(args(0).asInstanceOf[P1], args(1).asInstanceOf[P2]) }, 
          Seq(arg1, arg2)))
        )
  }
  
  /**
   * Creates a copy of this BeanDescriptor using given constructor.
   */
  def withConstructor[P1, P2, P3](
      ctor: (P1, P2, P3) => AnyRef, 
      arg1: Pair[String, Option[() => P1]], 
      arg2: Pair[String, Option[() => P2]],
      arg3: Pair[String, Option[() => P3]]): BeanDescriptor = {
    BeanDescriptor(
        manifest,
        properties.view.map(_.resetValueConvertor()).map(_.model),
        Some(ConstructorModel({ args: Array[AnyRef] => 
          ctor(args(0).asInstanceOf[P1], args(1).asInstanceOf[P2], args(2).asInstanceOf[P3]) }, 
          Seq(arg1, arg2, arg3)))
        )
  }
  
  /**
   * Creates a copy of this BeanDescriptor using given constructor.
   */
  def withConstructor[P1, P2, P3, P4](
      ctor: (P1, P2, P3, P4) => AnyRef, 
      arg1: Pair[String, Option[() => P1]], 
      arg2: Pair[String, Option[() => P2]],
      arg3: Pair[String, Option[() => P3]],
      arg4: Pair[String, Option[() => P4]]): BeanDescriptor = {
    BeanDescriptor(
        manifest,
        properties.view.map(_.resetValueConvertor()).map(_.model),
        Some(ConstructorModel({ args: Array[AnyRef] => 
          ctor(args(0).asInstanceOf[P1], args(1).asInstanceOf[P2], args(2).asInstanceOf[P3], args(3).asInstanceOf[P4]) }, 
          Seq(arg1, arg2, arg3, arg4)))
        )
  }
  
  /**
   * Creates a copy of this BeanDescriptor using given constructor.
   */
  def withConstructor[P1, P2, P3, P4, P5](
      ctor: (P1, P2, P3, P4, P5) => AnyRef, 
      arg1: Pair[String, Option[() => P1]], 
      arg2: Pair[String, Option[() => P2]],
      arg3: Pair[String, Option[() => P3]],
      arg4: Pair[String, Option[() => P4]],
      arg5: Pair[String, Option[() => P5]]): BeanDescriptor = {
    BeanDescriptor(
        manifest,
        properties.view.map(_.resetValueConvertor()).map(_.model),
        Some(ConstructorModel({ args: Array[AnyRef] => 
          ctor(args(0).asInstanceOf[P1], args(1).asInstanceOf[P2], args(2).asInstanceOf[P3], args(3).asInstanceOf[P4], args(4).asInstanceOf[P5]) }, 
          Seq(arg1, arg2, arg3, arg4, arg5)))
        )
  }
  
  /**
   * Creates a copy of this BeanDescriptor using given constructor.
   * 
   * Only bean properties can be used as constructor parameters.
   * 
   * @param ctor Bean constructor. Size of the array supplied to this function at runtime
   *             will be equal to the number of property names provided. Actual types will
   *             be assignable to the corresponding property types. If type conversion was
   *             applied to the properties '''before''' injection of new constructor,
   *             converted (new) types will be supplied to the constructor. 
   *             
   * @param args Property names used as constructor parameters with optional default values.
   * 
   * See convenience withConstructor methods with up to 5 arguments in the constructor function.
   */
  def withConstructor(ctor: Array[AnyRef] => AnyRef, args: Pair[String, Option[() => Any]]*) = {
    BeanDescriptor(
        manifest,
        properties.view.map(_.resetValueConvertor()).map(_.model),
        Some(ConstructorModel(ctor, args)))
  }
  
  override def toString = manifest.toString
}

object BeanDescriptor {

  def apply[T <: AnyRef](_beanType: ScalaType): BeanDescriptor = apply[T](ManifestFactory.manifestOf(_beanType))
  def apply[T <: AnyRef](_manifest: Manifest[_]): BeanDescriptor = {
    val propertyModels = BeanIntrospector.introspectProperties(_manifest)
    val ctorModel = ConstructorModel.introspectDefaultConstructor(_manifest.erasure)

    apply(_manifest, propertyModels, ctorModel)
  }

  private[scalabeans] def apply(
    _manifest: Manifest[_],
    propertyModels: Seq[PropertyDescriptor.Model],
    ctorModelO: Option[ConstructorModel]) = {

    var mutablePropertyIndex = 0

    val _properties =
      for (propertyModel <- propertyModels) yield {

        val (ctorParameterIndex, defaultValue) = ctorModelO match {
          case Some(ctorModel) =>
            var result = ctorModel.indexOfParameter(propertyModel.name)
            if (result < 0 && propertyModel.isInherited)
              result = ctorModel.indexOfParameter("_" + propertyModel.name)

            (result, if (result < 0) None else ctorModel.parameters(result)._2)

          case None => (-1, None)
        }

        val property = PropertyDescriptor(propertyModel, mutablePropertyIndex, ctorParameterIndex, defaultValue)

        if (!propertyModel.setter.isEmpty && ctorParameterIndex < 0) {
          mutablePropertyIndex += 1
        }

        property
      }

    new BeanDescriptor {
      val manifest = _manifest
      val properties = _properties
      val constructor = ctorModelO map { ctorModel =>
        new BeanConstructor {
          def newInstance(args: Array[AnyRef]): AnyRef = ctorModel.ctor(args)
          val arity = ctorModel.parameters.size
        }
      }
    }

  }
}