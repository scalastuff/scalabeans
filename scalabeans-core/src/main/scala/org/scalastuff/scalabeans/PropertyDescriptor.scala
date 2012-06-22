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

import java.lang.reflect.{ AnnotatedElement, Modifier, Method, Field }
import Preamble._
import org.scalastuff.scalabeans.types._
import org.scalastuff.scalabeans.sig.ScalaTypeCompiler
import org.scalastuff.util.Converter
import org.scalastuff.util.Rules

trait PropertyDescriptor {

  type ThisType <: PropertyDescriptor

  /**
   * Property name
   */
  def name = model.name
  def rename(newName: String) = clone(model.copy(name = newName))

  /**
   * Shows if this property value can be changed
   */
  def mutable: Boolean

  /**
   * Metamodel of the property value
   */
  def metamodel = model.metamodel

  def rewriteMetamodel(rules: Rules[Metamodel]) = clone(model.rewriteMetamodel(rules))
  
  def updateMetamodel(_metamodel: Metamodel) = rewriteMetamodel(metamodelRules {
    case Metamodel(model.metamodel.visibleMetamodel.scalaType) => LeafMetamodel(_metamodel)
  })
  
  /**
   * Type of the visible value
   */
  def visibleType = metamodel.visibleMetamodel.scalaType
  
  /**
   * Original type of the property value (without any conversions)
   */
  def underlyingType = metamodel.scalaType

  /**
   * Unique id of the property within the bean.
   *
   * Current implementation assigns tag to sequential number in the order of appearance (superclass first).
   *
   * Useful for serialization formats using property ids instead of names (like protobuf).
   */
  def tag: Int = model.tag

  /**
   * Creates a copy of this PropertyDescriptor using new tag value
   */
  def retag(newTag: Int) = clone(model.copy(tag = newTag))

  /**
   * Gets property value from given bean.
   *
   * If metamodel of the property value contains conversions, this method returns converted (visible) value.
   * This method is inefficient in cases when ContainerMetamodel has elementMetamodel with conversions. Better
   * approach is to get underlying container and convert elements. 
   * 
   * For example, when Array[Int] has to be converted to Array[String] this method will
   * perform actual conversion of underlying Array[Int] to visible Array[String].
   * 
   * Another side-effect of conversion is distortion of object references unless conversion functions are memos.
   *
   * @param obj bean object
   */
  def get[A](obj: AnyRef): A = {
    val underlying = model.getter(obj)
    metamodel.toVisible(underlying).asInstanceOf[A]
  }

  /**
   * Returns bean property value without any conversions possibly present in property value metamodel.
   */
  def getUnderlying[A](obj: AnyRef): A = model.getter(obj).asInstanceOf[A]

  /**
   * Looks if property is annotated with given annotation class and returns the annotation instance if found.
   */
  def findAnnotation[T <: java.lang.annotation.Annotation](implicit mf: Manifest[T]): Option[T] = model.findAnnotation(mf).asInstanceOf[Option[T]]

  /**
   * Type of the bean this property belongs to.
   */
  def beanType: ScalaType = model.beanType

  override def toString = "%s : %s // tag: %d".format(name, metamodel.scalaType.toString, tag)

  private[scalabeans] val model: PropertyDescriptor.Model

  protected[this] def clone(newModel: PropertyDescriptor.Model): ThisType
}

trait ImmutablePropertyDescriptor extends PropertyDescriptor {
  type ThisType <: ImmutablePropertyDescriptor

  val mutable = false

  override def toString = super.toString + ", readonly"
}

trait DeserializablePropertyDescriptor extends PropertyDescriptor {
  def index: Int
}

trait MutablePropertyDescriptor extends DeserializablePropertyDescriptor {
  type ThisType <: MutablePropertyDescriptor

  val mutable = true

  def setUnderlying(obj: AnyRef, value: Any): Unit = model.setter.get.apply(obj, value)

  def set(obj: AnyRef, value: Any): Unit = {
    metamodel match {
      case cv: ConvertedMetamodel =>
        setUnderlying(obj, cv.toUnderlying(value))
      case _ =>
        setUnderlying(obj, value)
    }
  }

  //  def javaSet(obj: AnyRef, value: AnyRef): Unit
}

trait ConstructorParameter extends DeserializablePropertyDescriptor {
  type ThisType <: ConstructorParameter

  /**
   * Default value as defined in constructor.
   *
   * Actually it can be dynamic, so it is a function, not a value.
   */
  def defaultValue: Option[() => Any] = model.defaultValue
  def setDefaultValue(newDefaultValue: Option[() => Any]) = clone(model.copy(defaultValue = newDefaultValue))
}

object PropertyDescriptor {
  private[scalabeans] def apply(model: Model, index: Int, ctorParameterIndex: Int, _defaultValue: Option[() => Any]): PropertyDescriptor = {

    model.setter match {
      case Some(_) =>
        if (ctorParameterIndex < 0) {
          mutable(model, index)
        } else {
          mutableCP(model.copy(defaultValue = _defaultValue), ctorParameterIndex)
        }
      case None =>
        if (ctorParameterIndex < 0) {
          immutable(model)
        } else {
          immutableCP(model.copy(defaultValue = _defaultValue), ctorParameterIndex)
        }
    }
  }

  def unapply(pd: PropertyDescriptor) = Some(pd.name, pd.visibleType)

  protected def immutable(_model: Model) = {
    trait ClonableImpl {
      type ThisType = ImmutablePropertyDescriptor

      def clone(newModel: Model): ThisType = new ImmutablePropertyDescriptor with ClonableImpl {
        val model = newModel
      }
    }

    new ImmutablePropertyDescriptor with ClonableImpl {
      val model = _model
    }
  }

  protected def immutableCP(_model: Model, _index: Int) = {
    trait ClonableImpl {
      type ThisType = ImmutablePropertyDescriptor with ConstructorParameter

      val index = _index

      def clone(newModel: Model): ThisType = new ImmutablePropertyDescriptor with ConstructorParameter with ClonableImpl {
        val model = newModel
      }
    }

    new ImmutablePropertyDescriptor with ConstructorParameter with ClonableImpl {
      val model = _model
    }
  }

  protected def mutable(_model: Model, _index: Int) = {
    trait ClonableImpl {
      type ThisType = MutablePropertyDescriptor

      val index = _index

      def clone(newModel: Model): ThisType = new MutablePropertyDescriptor with ClonableImpl {
        val model = newModel
      }
    }

    new MutablePropertyDescriptor with ClonableImpl {
      val model = _model
    }
  }

  protected def mutableCP(_model: Model, _index: Int) = {
    trait ClonableImpl {
      type ThisType = MutablePropertyDescriptor with ConstructorParameter

      val index = _index

      def clone(newModel: Model): ThisType = new MutablePropertyDescriptor with ConstructorParameter with ClonableImpl {
        val model = newModel
      }
    }

    new MutablePropertyDescriptor with ConstructorParameter with ClonableImpl {
      val model = _model
    }
  }

  private[scalabeans] class Model(
    val beanType: ScalaType,
    val name: String,
    _metamodel: => Metamodel,
    _ctorArgMetamodel: => Metamodel,
    val tag: Int,
    val getter: (AnyRef => Any),
    val setter: Option[(AnyRef, Any) => Unit],
    val defaultValue: Option[() => Any] = None,
    val findAnnotation: (Manifest[_] => Option[_]) = { _ => None },
    val isInherited: Boolean = false) {

    def this(
      beanType: ScalaType,
      name: String,
      _metamodel: => Metamodel,
      tag: Int,
      getter: (AnyRef => Any),
      setter: Option[(AnyRef, Any) => Unit],
      defaultValue: Option[() => Any] = None,
      findAnnotation: (Manifest[_] => Option[_]) = { _ => None },
      isInherited: Boolean = false) = this(beanType,
      name,
      _metamodel,
      _metamodel,
      tag,
      getter,
      setter,
      defaultValue,
      findAnnotation,
      isInherited)

    def copy(
      name: String = this.name,
      tag: Int = this.tag,
      getter: (AnyRef => Any) = this.getter,
      setter: Option[(AnyRef, Any) => Unit] = this.setter,
      defaultValue: Option[() => Any] = this.defaultValue) =
      new Model(
        this.beanType,
        name,
        _metamodel,
        _ctorArgMetamodel,
        tag,
        getter,
        setter,
        defaultValue,
        this.findAnnotation,
        this.isInherited)
    
    lazy val metamodel = _metamodel
    lazy val ctorArgMetamodel = {
      val result = _ctorArgMetamodel
      require(result.visibleMetamodel.scalaType == metamodel.visibleMetamodel.scalaType,
          "Cannot calculate metamodel of constructor argument: " +
          "calculated visible type %s doesn't match with visible property value type %s. " +
          "This can be a result of previous metamodel rewrites.".
          format(result.visibleMetamodel.scalaType, metamodel.visibleMetamodel.scalaType))
      
      result
    }
    
    def rewriteMetamodel(rules: Rules[Metamodel]) = new Model(
        this.beanType,
        name,
        metamodel rewrite rules,
        ctorArgMetamodel rewrite rules,
        tag,
        getter,
        setter,
        defaultValue,
        this.findAnnotation,
        this.isInherited)
    
    def resetCtorArgMetamodel = new Model(
        this.beanType,
        name,
        metamodel,
        ctorArgMetamodel match {
          case cv: ConvertedMetamodel => cv.visibleMetamodel
          case _ => ctorArgMetamodel
        },
        tag,
        getter,
        setter,
        defaultValue,
        this.findAnnotation,
        this.isInherited) 
  }

  private[scalabeans] object Model {
    def apply(_beanType: ScalaType, _tag: Int, field: Option[Field], getter: Option[Method], setter: Option[Method]): Model = {
      val findAnnotation = { mf: Manifest[_] =>
        def findAnnotationHere(annotated: AnnotatedElement) = Option(annotated.getAnnotation(mf.erasure.asInstanceOf[Class[java.lang.annotation.Annotation]]))

        def findFieldAnnotation = field flatMap findAnnotationHere
        def findGetterAnnotation = getter flatMap findAnnotationHere
        def findSetterAnnotation = setter flatMap findAnnotationHere

        findFieldAnnotation orElse findGetterAnnotation orElse findSetterAnnotation
      }

      val accessible = field orElse getter get
      val isInherited = accessible.getDeclaringClass() != _beanType.erasure

      def immutableModelFromField(field: Field, typeHint: Option[ScalaType]) = new Model(
        _beanType,
        field.getName,
        metamodelOf(typeHint getOrElse scalaTypeOf(field.getGenericType)),
        _tag,
        field.get _,
        None,
        None,
        findAnnotation,
        isInherited)

      def mutableModelFromField(field: Field, typeHint: Option[ScalaType]) = new Model(
        _beanType,
        field.getName,
        metamodelOf(typeHint getOrElse scalaTypeOf(field.getGenericType)),
        _tag,
        field.get _,
        Some(field.set _),
        None,
        findAnnotation,
        isInherited)

      def modelFromGetter(getter: Method, typeHint: Option[ScalaType]) = new Model(
        _beanType,
        getter.getName,
        metamodelOf(typeHint getOrElse scalaTypeOf(getter.getGenericReturnType)),
        _tag,
        getter.invoke(_),
        None,
        None,
        findAnnotation,
        isInherited)

      def modelFromGetterSetter(getter: Method, setter: Method, typeHint: Option[ScalaType]) = new Model(
        _beanType,
        getter.getName,
        metamodelOf(typeHint getOrElse scalaTypeOf(getter.getGenericReturnType)),
        _tag,
        getter.invoke(_),
        Some({ (obj: AnyRef, value: Any) => setter.invoke(obj, value.asInstanceOf[AnyRef]) }),
        None,
        findAnnotation,
        isInherited)

      val propertyTypeHint = _beanType match {
        //case tt: TupleType => Some(tt.arguments(_tag - 1))
        case _beanType @ _ =>
          for {
            member <- field orElse getter
            classInfo <- ScalaTypeCompiler.classInfoOf(_beanType)
            typeHint <- classInfo.findPropertyType(member.getName)
          } yield typeHint
      }

      ((field, getter, setter): @unchecked) match {
        case (Some(f), None, None) =>
          if (Modifier.isFinal(f.getModifiers)) immutableModelFromField(f, propertyTypeHint)
          else mutableModelFromField(f, propertyTypeHint)

        case (Some(f), Some(g), None) =>
          modelFromGetter(g, propertyTypeHint)

        case (_, Some(g), Some(s)) =>
          modelFromGetterSetter(g, s, propertyTypeHint)
      }
    }
  }

}