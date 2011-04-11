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

import java.lang.reflect.{AnnotatedElement, Modifier, Method, Field}
import Preamble._
import org.scalastuff.scalabeans.types._

abstract class PropertyDescriptor {

  def name: String

  def mutable: Boolean

  def scalaType: ScalaType

  def tag: Int

  def get[A](obj: AnyRef): A

  def findAnnotation[T <: java.lang.annotation.Annotation](implicit mf: Manifest[T]): Option[T]

  def beanType: ScalaType

  //  def javaType: java.lang.reflect.Type
  //
  //  def javaGet(obj: AnyRef): AnyRef

  override def toString = "%s : %s // tag: %d".format(name, scalaType.toString, tag)
}

trait ImmutablePropertyDescriptor extends PropertyDescriptor {
  val mutable = false

  override def toString = super.toString + ", readonly"
}

trait DeserializablePropertyDescriptor extends PropertyDescriptor {
  def index: Int
}

trait MutablePropertyDescriptor extends DeserializablePropertyDescriptor {
  val mutable = true

  def set(obj: AnyRef, value: Any): Unit

  //  def javaSet(obj: AnyRef, value: AnyRef): Unit
}

trait ConstructorParameter extends DeserializablePropertyDescriptor {

  def defaultValue: Option[() => Any]

}

object PropertyDescriptor {
  def apply(_beanType: ScalaType, _tag: Int, _index: Int, field: Option[Field], getter: Option[Method], setter: Option[Method], ctorParameterIndex: Int, defaultValueMethod: Option[Method]): PropertyDescriptor = {
    trait ConstructorParameterImpl extends ConstructorParameter {
      val index = ctorParameterIndex

      def defaultValue = defaultValueMethod map {
        method => {
          () => method.invoke(null)
        }
      }
    }

    trait PropertyDescriptorImpl extends PropertyDescriptor {
      val tag = _tag
      val beanType = _beanType

      def findAnnotation[T <: java.lang.annotation.Annotation](implicit mf: Manifest[T]): Option[T] = {
        def findAnnotationHere(annotated: AnnotatedElement): Option[T] = Option(annotated.getAnnotation(mf.erasure.asInstanceOf[Class[T]]))

        def findFieldAnnotation = field flatMap findAnnotationHere
        def findGetterAnnotation = getter flatMap findAnnotationHere
        def findSetterAnnotation = setter flatMap findAnnotationHere

        findFieldAnnotation orElse findGetterAnnotation orElse findSetterAnnotation
      }
    }

    def immutableFieldPropertyDescriptor(field: Field, typeHint: Option[ScalaType], ctorParameterIndex: Int, defaultValueMethod: Option[Method]) = {
      if (ctorParameterIndex < 0) {
        new ImmutableFieldPropertyDescriptor(field, typeHint)
      } else {
        new ImmutableFieldPropertyDescriptor(field, typeHint) with ConstructorParameterImpl
      }
    }

    def mutableFieldPropertyDescriptor(field: Field, ctorParameterIndex: Int, defaultValueMethod: Option[Method]) = {
      if (ctorParameterIndex < 0) {
        new MutableFieldPropertyDescriptor(field) { def index = _index }
      } else {
        new MutableFieldPropertyDescriptor(field) with ConstructorParameterImpl
      }
    }

    def getterPropertyDescriptor(getter: Method, typeHint: Option[ScalaType], ctorParameterIndex: Int, defaultValueMethod: Option[Method]) = {
      if (ctorParameterIndex < 0) {
        new GetterPropertyDescriptor(getter, typeHint)
      } else {
        new GetterPropertyDescriptor(getter, typeHint) with ConstructorParameterImpl
      }
    }

    def getterSetterPropertyDescriptor(getter: Method, setter: Method, ctorParameterIndex: Int, defaultValueMethod: Option[Method]) = {
      if (ctorParameterIndex < 0) {
        new GetterSetterPropertyDescriptor(getter, setter) { def index = _index }
      } else {
        new GetterSetterPropertyDescriptor(getter, setter) with ConstructorParameterImpl
      }
    }

    //
    // Field Property Descriptors
    //

    abstract class FieldPropertyDescriptor(field: Field, typeHint: Option[ScalaType] = None) extends PropertyDescriptorImpl {
      field.setAccessible(true)

      val name = field.getName
      val scalaType = typeHint getOrElse scalaTypeOf(field.getGenericType)
      val manifest = ManifestFactory.manifestOf(field.getGenericType)

      def get[A](obj: AnyRef) = field.get(obj).asInstanceOf[A]
    }

    class ImmutableFieldPropertyDescriptor(field: Field, typeHint: Option[ScalaType])
      extends FieldPropertyDescriptor(field, typeHint) with ImmutablePropertyDescriptor

    abstract class MutableFieldPropertyDescriptor(field: Field)
      extends FieldPropertyDescriptor(field, None) with MutablePropertyDescriptor {

      def set(obj: AnyRef, value: Any) = field.set(obj, value)
    }

    //
    // Method Property Descriptors
    //

    abstract class MethodPropertyDescriptor(getter: Method, typeHint: Option[ScalaType] = None) extends PropertyDescriptorImpl {
      getter.setAccessible(true)

      val name = getter.getName
      val scalaType = typeHint getOrElse scalaTypeOf(getter.getGenericReturnType)
      val manifest = ManifestFactory.manifestOf(getter.getGenericReturnType)

      def get[A](obj: AnyRef) = getter.invoke(obj).asInstanceOf[A]

    }

    class GetterPropertyDescriptor(getter: Method, typeHint: Option[ScalaType]) extends MethodPropertyDescriptor(getter, typeHint) with ImmutablePropertyDescriptor

    abstract class GetterSetterPropertyDescriptor(getter: Method, setter: Method) extends MethodPropertyDescriptor(getter, None) with MutablePropertyDescriptor {
      setter.setAccessible(true)

      def set(obj: AnyRef, value: Any): Unit = setter.invoke(obj, value.asInstanceOf[AnyRef])
    }

    val propertyTypeHint = _beanType match {
      case tt: TupleType => Some(tt.arguments(_tag - 1))
      case _ => None
    }

    ((field, getter, setter) : @unchecked) match {
      case (Some(f), None, None) =>
        if (Modifier.isFinal(f.getModifiers)) immutableFieldPropertyDescriptor(f, propertyTypeHint, ctorParameterIndex, defaultValueMethod)
        else mutableFieldPropertyDescriptor(f, ctorParameterIndex, defaultValueMethod)

      case (Some(f), Some(g), None) =>
        getterPropertyDescriptor(g, propertyTypeHint, ctorParameterIndex, defaultValueMethod)

      case (_, Some(g), Some(s)) =>
        getterSetterPropertyDescriptor(g, s, ctorParameterIndex, defaultValueMethod)
    }
  }
}