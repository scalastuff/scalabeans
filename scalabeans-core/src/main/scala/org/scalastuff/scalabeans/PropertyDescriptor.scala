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
import org.scalastuff.scalabeans.converters.Converter
import org.scalastuff.util.Rules

trait PropertyDescriptor {

  type ThisType <: PropertyDescriptor

  /**
   * Property name
   */
  def name = model.name
  
  /**
   * Rename the property
   */
  def withName(newName: String) = clone(model.copy(name = newName))

  /**
   * Shows if this property value can be changed
   */
  def mutable: Boolean

  /**
   * Meta model of the property value
   */
  def typeMetaModel = model.metaModel

  def rewriteTypeMetaModel(rules: Rules[MetaModel]) = clone(model.copy(metaModel = model.metaModel.rewrite(rules)))

  def withTypeMetaModel(_metaModel: => MetaModel) = {
    require(typeMetaModel.scalaType == _metaModel.scalaType,
      "Cannot update property metaModel: expect meta model for type %s, found %s".
        format(typeMetaModel.scalaType, _metaModel.scalaType))
        
    clone(model.copy(metaModel =_metaModel))
  }

  /**
   * Type of the visible value
   */
  def visibleType = typeMetaModel.visibleMetaModel.scalaType

  /**
   * Original type of the property value (without any conversions)
   */
  def underlyingType = typeMetaModel.scalaType

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
  def withTag(newTag: Int) = clone(model.copy(tag = newTag))

  /**
   * Gets property value from given bean.
   *
   * If meta model of the property value contains conversions, this method returns converted (visible) value.
   * This method is inefficient in cases when ContainerMetaModel has elementMetaModel with conversions. Better
   * approach is to get underlying container and convert elements.
   *
   * For example, when Array[Int] has to be converted to Array[String] this method will
   * perform actual conversion of underlying Array[Int] to visible Array[String].
   *
   * Another side-effect of conversion is distortion of object references unless conversion functions are memos.
   *
   * @param obj bean object
   */
  def getVisible[A](obj: AnyRef): A = {
    val underlying = model.getter(obj)
    typeMetaModel.toVisible(underlying).asInstanceOf[A]
  }

  /**
   * Returns bean property value without any conversions possibly present in property value meta model.
   */
  def get[A](obj: AnyRef): A = model.getter(obj).asInstanceOf[A]

  /**
   * Looks if property is annotated with given annotation class and returns the annotation instance if found.
   */
  def findAnnotation[T <: java.lang.annotation.Annotation](implicit mf: Manifest[T]): Option[T] = model.findAnnotation(mf).asInstanceOf[Option[T]]

  /**
   * Type of the bean this property belongs to.
   */
  def beanType: ScalaType = model.beanType

  override def toString = "%s : %s // tag: %d".format(name, visibleType, tag)

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

  def set(obj: AnyRef, value: Any): Unit = model.setter.get.apply(obj, value)

  def setVisible(obj: AnyRef, value: Any): Unit = set(obj, typeMetaModel.toUnderlying(value))
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
    _metaModel: => MetaModel,
    val tag: Int,
    val getter: (AnyRef => Any),
    val setter: Option[(AnyRef, Any) => Unit],
    val defaultValue: Option[() => Any] = None,
    val findAnnotation: (Manifest[_] => Option[_]) = { _ => None },
    val isInherited: Boolean = false) {

    def copy(
      name: String = this.name,
      tag: Int = this.tag,
      metaModel: => MetaModel = _metaModel,
      getter: (AnyRef => Any) = this.getter,
      setter: Option[(AnyRef, Any) => Unit] = this.setter,
      defaultValue: Option[() => Any] = this.defaultValue) =
      new Model(
        this.beanType,
        name,
        metaModel,
        tag,
        getter,
        setter,
        defaultValue,
        this.findAnnotation,
        this.isInherited)

    lazy val metaModel = _metaModel
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
        metaModelOf(typeHint getOrElse scalaTypeOf(field.getGenericType)),
        _tag,
        field.get _,
        None,
        None,
        findAnnotation,
        isInherited)

      def mutableModelFromField(field: Field, typeHint: Option[ScalaType]) = new Model(
        _beanType,
        field.getName,
        metaModelOf(typeHint getOrElse scalaTypeOf(field.getGenericType)),
        _tag,
        field.get _,
        Some(field.set _),
        None,
        findAnnotation,
        isInherited)

      def modelFromGetter(getter: Method, typeHint: Option[ScalaType]) = new Model(
        _beanType,
        getter.getName,
        metaModelOf(typeHint getOrElse scalaTypeOf(getter.getGenericReturnType)),
        _tag,
        getter.invoke(_),
        None,
        None,
        findAnnotation,
        isInherited)

      def modelFromGetterSetter(getter: Method, setter: Method, typeHint: Option[ScalaType]) = new Model(
        _beanType,
        getter.getName,
        metaModelOf(typeHint getOrElse scalaTypeOf(getter.getGenericReturnType)),
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