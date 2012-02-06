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
   * Converts property value from A to B
   */
  def convertValue[A, B : Manifest](to: A => B, from: B => A) = {
    val newScalaType = scalaTypeOf[B]
    clone(model.copy(scalaType = newScalaType, 
        getter = { obj: AnyRef => to(model.getter(obj).asInstanceOf[A]) },
        setter = { (obj: AnyRef, b: Any) => model.setter(obj, from(b.asInstanceOf[B]))}))
  }
  
  protected[scalabeans] def updateScalaType(newScalaType: ScalaType) = clone(model.copy(scalaType = newScalaType))

  /**
   * Unique id of the property within the bean.
   *
   * Current implementation assigns tag to sequential number in the order of appearance (superclass first).
   *
   * Useful for serialization formats using property ids instead of names (like protobuf).
   */
  def tag = model.tag

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
  def beanManifest: Manifest[_]

  //  def javaType: java.lang.reflect.Type
  //
  //  def javaGet(obj: AnyRef): AnyRef

  override def toString = "%s : %s // tag: %d".format(name, scalaType.toString, tag)

  protected[this] val model: PropertyDescriptor.PropertyModel

  protected[this] def clone(newModel: PropertyDescriptor.PropertyModel): ThisType
}

trait ImmutablePropertyDescriptor extends PropertyDescriptor {
  type ThisType <: ImmutablePropertyDescriptor

  val mutable = false

  override def toString = super.toString + ", readonly"
}

trait DeserializablePropertyDescriptor extends PropertyDescriptor {
  def index = model.index
}

trait MutablePropertyDescriptor extends DeserializablePropertyDescriptor {
  type ThisType <: MutablePropertyDescriptor

  val mutable = true

  def set(obj: AnyRef, value: Any): Unit = model.setter(obj, value)

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

case class ValueConvertor[A, B](to: A => B, from: B => A)

object PropertyDescriptor {
  def apply(_beanMF: Manifest[_], _tag: Int, _index: Int, field: Option[Field], getter: Option[Method], setter: Option[Method], ctorParameterIndex: Int, defaultValueMethod: Option[Method]): PropertyDescriptor = {
    val defaultValue = defaultValueMethod map { method =>
      { () => method.invoke(null) }
    }

    val findAnnotation = { mf: Manifest[_] =>
      def findAnnotationHere(annotated: AnnotatedElement) = Option(annotated.getAnnotation(mf.erasure.asInstanceOf[Class[java.lang.annotation.Annotation]]))

      def findFieldAnnotation = field flatMap findAnnotationHere
      def findGetterAnnotation = getter flatMap findAnnotationHere
      def findSetterAnnotation = setter flatMap findAnnotationHere

      findFieldAnnotation orElse findGetterAnnotation orElse findSetterAnnotation
    }

    def modelFromField(field: Field, typeHint: Option[ScalaType], propertyIndex: Int) = PropertyModel(
      field.getName,
      typeHint getOrElse scalaTypeOf(field.getGenericType),
      _tag,
      propertyIndex,
      field.get,
      field.set,
      defaultValue,
      findAnnotation)
      
    def modelFromGetter(getter: Method, typeHint: Option[ScalaType], propertyIndex: Int) = PropertyModel(
      getter.getName,
      typeHint getOrElse scalaTypeOf(getter.getGenericReturnType),
      _tag,
      propertyIndex,
      getter.invoke(_),
      null,
      defaultValue,
      findAnnotation)
      
    def modelFromGetterSetter(getter: Method, setter: Method, typeHint: Option[ScalaType], propertyIndex: Int) = PropertyModel(
      getter.getName,
      typeHint getOrElse scalaTypeOf(getter.getGenericReturnType),
      _tag,
      propertyIndex,
      getter.invoke(_),
      {(obj: AnyRef, value: Any) => setter.invoke(obj, value.asInstanceOf[AnyRef])},
      defaultValue,
      findAnnotation)
      
    def immutableFieldPropertyDescriptor(field: Field, typeHint: Option[ScalaType]) = {
      if (ctorParameterIndex < 0) {
        immutable(_beanMF, modelFromField(field, typeHint, _index))        
      } else {
        immutableCP(_beanMF, modelFromField(field, typeHint, ctorParameterIndex))
      }
    }

    def mutableFieldPropertyDescriptor(field: Field, typeHint: Option[ScalaType]) = {
      if (ctorParameterIndex < 0) {
        mutable(_beanMF, modelFromField(field, typeHint, _index))        
      } else {
        mutableCP(_beanMF, modelFromField(field, typeHint, ctorParameterIndex))
      }
    }

    def getterPropertyDescriptor(getter: Method, typeHint: Option[ScalaType]) = {
      if (ctorParameterIndex < 0) {
        immutable(_beanMF, modelFromGetter(getter, typeHint, _index))        
      } else {
        immutableCP(_beanMF, modelFromGetter(getter, typeHint, ctorParameterIndex))
      }
    }

    def getterSetterPropertyDescriptor(getter: Method, setter: Method, typeHint: Option[ScalaType]) = {
      if (ctorParameterIndex < 0) {
        mutable(_beanMF, modelFromGetterSetter(getter, setter, typeHint, _index))        
      } else {
        mutableCP(_beanMF, modelFromGetterSetter(getter, setter, typeHint, ctorParameterIndex))
      }
    }

    val propertyTypeHint = scalaTypeOf(_beanMF) match {
      case tt: TupleType => Some(tt.arguments(_tag - 1))
      case _beanType @ _ =>
        for {
          member <- field orElse getter
          classInfo <- ScalaTypeCompiler.classInfoOf(_beanType)
          typeHint <- classInfo.findPropertyType(member.getName)
        } yield typeHint
    }

    ((field, getter, setter): @unchecked) match {
      case (Some(f), None, None) =>
        if (Modifier.isFinal(f.getModifiers)) immutableFieldPropertyDescriptor(f, propertyTypeHint)
        else mutableFieldPropertyDescriptor(f, propertyTypeHint)

      case (Some(f), Some(g), None) =>
        getterPropertyDescriptor(g, propertyTypeHint)

      case (_, Some(g), Some(s)) =>
        getterSetterPropertyDescriptor(g, s, propertyTypeHint)
    }
  }

  protected def immutable(_beanManifest: Manifest[_], _model: PropertyModel) = {
    trait ClonableImpl {
      type ThisType = ImmutablePropertyDescriptor

      val beanManifest = _beanManifest

      def clone(newModel: PropertyModel): ThisType = new ImmutablePropertyDescriptor with ClonableImpl {
        val model = newModel
      }
    }

    new ImmutablePropertyDescriptor with ClonableImpl {
      val model = _model
    }
  }

  protected def immutableCP(_beanManifest: Manifest[_], _model: PropertyModel) = {
    trait ClonableImpl {
      type ThisType = ImmutablePropertyDescriptor with ConstructorParameter

      val beanManifest = _beanManifest

      def clone(newModel: PropertyModel): ThisType = new ImmutablePropertyDescriptor with ConstructorParameter with ClonableImpl {
        val model = newModel
      }
    }

    new ImmutablePropertyDescriptor with ConstructorParameter with ClonableImpl {
      val model = _model
    }
  }

  protected def mutable(_beanManifest: Manifest[_], _model: PropertyModel) = {
    trait ClonableImpl {
      type ThisType = MutablePropertyDescriptor

      val beanManifest = _beanManifest

      def clone(newModel: PropertyModel): ThisType = new MutablePropertyDescriptor with ClonableImpl {
        val model = newModel
      }
    }

    new MutablePropertyDescriptor with ClonableImpl {
      val model = _model
    }
  }

  protected def mutableCP(_beanManifest: Manifest[_], _model: PropertyModel) = {
    trait ClonableImpl {
      type ThisType = MutablePropertyDescriptor with ConstructorParameter

      val beanManifest = _beanManifest

      def clone(newModel: PropertyModel): ThisType = new MutablePropertyDescriptor with ConstructorParameter with ClonableImpl {
        val model = newModel
      }
    }

    new MutablePropertyDescriptor with ConstructorParameter with ClonableImpl {
      val model = _model
    }
  }

  protected case class PropertyModel(
    name: String,
    scalaType: ScalaType,
    tag: Int,
    index: Int,
    getter: (AnyRef => Any),
    setter: (AnyRef, Any) => Unit,
    defaultValue: Option[() => Any],
    findAnnotation: (Manifest[_] => Option[_]))

}