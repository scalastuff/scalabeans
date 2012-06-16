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
   * Type of the property value
   */
  def scalaType = model.scalaType

  /**
   * Creates a copy of this PropertyDescriptor with property value converted to another type.
   */
  def convertValue[A, B: Manifest](to: A => B, from: B => A) = {
    val newScalaType = scalaTypeOf[B]
    clone(model.copy(scalaType = newScalaType,
      getter = { obj: AnyRef => to(model.getter(obj).asInstanceOf[A]) },
      setter = model.setter map { setter => { (obj: AnyRef, b: Any) => setter(obj, from(b.asInstanceOf[B])) } },
      valueConvertor = model.valueConvertor.compose(Converter[A, B](to, from).asInstanceOf[Converter[Any, Any]])))
  }

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
   * Gets property value from given bean
   *
   * @param obj bean object
   */
  def get[A](obj: AnyRef): A = model.getter(obj).asInstanceOf[A]

  /**
   * Looks if property is annotated with given annotation class and returns the annotation instance if found.
   */
  def findAnnotation[T <: java.lang.annotation.Annotation](implicit mf: Manifest[T]): Option[T] = model.findAnnotation(mf).asInstanceOf[Option[T]]

  /**
   * Type of the bean this property belongs to.
   */
  def beanManifest: Manifest[_] = model.beanManifest

  //  def javaType: java.lang.reflect.Type
  //
  //  def javaGet(obj: AnyRef): AnyRef

  override def toString = "%s : %s // tag: %d".format(name, scalaType.toString, tag)

  private[scalabeans] val model: PropertyDescriptor.Model
  private[scalabeans] def updateScalaType(newScalaType: => ScalaType) = clone(model.copy(scalaType = newScalaType))
  private[scalabeans] def resetValueConvertor() = clone(model.copy(valueConvertor = PropertyDescriptor.ValueConvertorIdentity))

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

  def set(obj: AnyRef, value: Any): Unit = model.setter.get.apply(obj, value)

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

  def unapply(pd: PropertyDescriptor) = Some((pd.name, pd.scalaType))

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

  private[scalabeans] val ValueConvertorIdentity = Converter.identity[Any]
  private[scalabeans] class Model(
    val beanManifest: Manifest[_],
    val name: String,
    _scalaType: => ScalaType,
    val tag: Int,
    val getter: (AnyRef => Any),
    val setter: Option[(AnyRef, Any) => Unit],
    val defaultValue: Option[() => Any] = None,
    val findAnnotation: (Manifest[_] => Option[_]) = { _ => None},
    val isInherited: Boolean = false,
    val valueConvertor: Converter[Any, Any] = ValueConvertorIdentity) {
    def copy(
      name: String = this.name,
      scalaType: => ScalaType = _scalaType,
      tag: Int = this.tag,
      getter: (AnyRef => Any) = this.getter,
      setter: Option[(AnyRef, Any) => Unit] = this.setter,
      defaultValue: Option[() => Any] = this.defaultValue,
      valueConvertor: Converter[Any, Any] = this.valueConvertor) =
      new Model(
        this.beanManifest,
        name,
        scalaType,
        tag,
        getter,
        setter,
        defaultValue,
        this.findAnnotation,
        this.isInherited,
        valueConvertor)
    lazy val scalaType = _scalaType
  }

  object Model {
    def apply(_beanMF: Manifest[_], _tag: Int, field: Option[Field], getter: Option[Method], setter: Option[Method]): Model = {
      val findAnnotation = { mf: Manifest[_] =>
        def findAnnotationHere(annotated: AnnotatedElement) = Option(annotated.getAnnotation(mf.erasure.asInstanceOf[Class[java.lang.annotation.Annotation]]))

        def findFieldAnnotation = field flatMap findAnnotationHere
        def findGetterAnnotation = getter flatMap findAnnotationHere
        def findSetterAnnotation = setter flatMap findAnnotationHere

        findFieldAnnotation orElse findGetterAnnotation orElse findSetterAnnotation
      }

      val accessible = field orElse getter get
      val isInherited = accessible.getDeclaringClass() != _beanMF.erasure

      def immutableModelFromField(field: Field, typeHint: Option[ScalaType]) = new Model(
        _beanMF,
        field.getName,
        typeHint getOrElse scalaTypeOf(field.getGenericType),
        _tag,
        field.get,
        None,
        None,
        findAnnotation,
        isInherited)

      def mutableModelFromField(field: Field, typeHint: Option[ScalaType]) = new Model(
        _beanMF,
        field.getName,
        typeHint getOrElse scalaTypeOf(field.getGenericType),
        _tag,
        field.get,
        Some(field.set),
        None,
        findAnnotation,
        isInherited)

      def modelFromGetter(getter: Method, typeHint: Option[ScalaType]) = new Model(
        _beanMF,
        getter.getName,
        typeHint getOrElse scalaTypeOf(getter.getGenericReturnType),
        _tag,
        getter.invoke(_),
        None,
        None,
        findAnnotation,
        isInherited)

      def modelFromGetterSetter(getter: Method, setter: Method, typeHint: Option[ScalaType]) = new Model(
        _beanMF,
        getter.getName,
        typeHint getOrElse scalaTypeOf(getter.getGenericReturnType),
        _tag,
        getter.invoke(_),
        Some({ (obj: AnyRef, value: Any) => setter.invoke(obj, value.asInstanceOf[AnyRef]) }),
        None,
        findAnnotation,
        isInherited)

      val propertyTypeHint = scalaTypeOf(_beanMF) match {
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