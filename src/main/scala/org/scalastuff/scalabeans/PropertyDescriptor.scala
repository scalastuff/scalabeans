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

abstract class PropertyDescriptor {

  /**
   * Property name
   */
  def name: String

  /**
   * Shows if this property value can be changed
   */
  def mutable: Boolean

  /**
   * Type of the property value
   */
  def scalaType: ScalaType

  /**
   * Unique id of the property within the bean. 
   * 
   * Current implementation assigns tag to sequential number in the order of appearance (superclass first).
   * 
   * Useful for serialization formats using property ids instead of names (like protobuf).
   */
  def tag: Int

  /**
   * Gets property value from given bean
   * 
   * @param obj bean object
   */
  def get[A](obj: AnyRef): A

  /**
   * Looks if property is annotated with given annotation class and returns the annotation instance if found.
   */
  def findAnnotation[T <: java.lang.annotation.Annotation](implicit mf: Manifest[T]): Option[T]

  /**
   * Type of the bean this property belongs to.
   */
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
  /**
   * Default value as defined in constructor.
   * 
   * Actually it can be dynamic, so it is a function, not a value. 
   */
  def defaultValue: Option[() => Any]

  def index: Int
}

trait FieldPropertyDescriptor extends PropertyDescriptor {
  def field: Field
}

trait GetterPropertyDescriptor extends PropertyDescriptor {
  def getter: Method
}

trait GetterSetterPropertyDescriptor extends GetterPropertyDescriptor {
  def setter: Method
}

object PropertyDescriptor {
  def apply(_beanType: ScalaType, _tag: Int, _index: Int, field: Option[Field], getter: Option[Method], setter: Option[Method], ctorParameterIndex: Int, defaultValueMethod: Option[Method]): PropertyDescriptor = {
    trait ConstructorParameterImpl extends ConstructorParameter {
      val index = ctorParameterIndex

      def defaultValue = defaultValueMethod map { method =>
        { () =>
          method.invoke(null)
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

    def immutableFieldPropertyDescriptor(field: Field, typeHint: Option[ScalaType], ctorParameterIndex: Int) = {
      if (ctorParameterIndex < 0) {
        new ImmutableFieldPropertyDescriptor(field, typeHint)
      } else {
        new ImmutableFieldPropertyDescriptor(field, typeHint) with ConstructorParameterImpl
      }
    }

    def mutableFieldPropertyDescriptor(field: Field, typeHint: Option[ScalaType], ctorParameterIndex: Int) = {
      if (ctorParameterIndex < 0) {
        new MutableFieldPropertyDescriptor(field, typeHint) { def index = _index }
      } else {
        new MutableFieldPropertyDescriptor(field, typeHint) with ConstructorParameterImpl
      }
    }

    def getterPropertyDescriptor(getter: Method, typeHint: Option[ScalaType], ctorParameterIndex: Int) = {
      if (ctorParameterIndex < 0) {
        new GetterPropertyDescriptorImpl(getter, typeHint)
      } else {
        new GetterPropertyDescriptorImpl(getter, typeHint) with ConstructorParameterImpl
      }
    }

    def getterSetterPropertyDescriptor(getter: Method, setter: Method, typeHint: Option[ScalaType], ctorParameterIndex: Int) = {
      if (ctorParameterIndex < 0) {
        new GetterSetterPropertyDescriptorImpl(getter, setter, typeHint) { def index = _index }
      } else {
        new GetterSetterPropertyDescriptorImpl(getter, setter, typeHint) with ConstructorParameterImpl
      }
    }

    //
    // Field Property Descriptors
    //

    abstract class FieldPropertyDescriptorImpl(val field: Field, typeHint: Option[ScalaType] = None) extends PropertyDescriptorImpl with FieldPropertyDescriptor {
      field.setAccessible(true)

      val name = field.getName
      val scalaType = typeHint getOrElse scalaTypeOf(field.getGenericType)
      val manifest = ManifestFactory.manifestOf(field.getGenericType)

      def get[A](obj: AnyRef) = field.get(obj).asInstanceOf[A]
    }

    class ImmutableFieldPropertyDescriptor(field: Field, typeHint: Option[ScalaType])
      extends FieldPropertyDescriptorImpl(field, typeHint) with ImmutablePropertyDescriptor

    abstract class MutableFieldPropertyDescriptor(field: Field, typeHint: Option[ScalaType])
      extends FieldPropertyDescriptorImpl(field, typeHint) with MutablePropertyDescriptor {

      def set(obj: AnyRef, value: Any) = field.set(obj, value)
    }

    //
    // Method Property Descriptors
    //

    abstract class MethodPropertyDescriptor(val getter: Method, typeHint: Option[ScalaType] = None) extends PropertyDescriptorImpl with GetterPropertyDescriptor {
      getter.setAccessible(true)

      val name = getter.getName
      val scalaType = typeHint getOrElse scalaTypeOf(getter.getGenericReturnType)
      val manifest = ManifestFactory.manifestOf(getter.getGenericReturnType)

      def get[A](obj: AnyRef) = getter.invoke(obj).asInstanceOf[A]

    }

    class GetterPropertyDescriptorImpl(getter: Method, typeHint: Option[ScalaType]) extends MethodPropertyDescriptor(getter, typeHint) with ImmutablePropertyDescriptor

    abstract class GetterSetterPropertyDescriptorImpl(getter: Method, val setter: Method, typeHint: Option[ScalaType]) extends MethodPropertyDescriptor(getter, typeHint) with MutablePropertyDescriptor with GetterSetterPropertyDescriptor {
      setter.setAccessible(true)

      def set(obj: AnyRef, value: Any): Unit = setter.invoke(obj, value.asInstanceOf[AnyRef])
    }

    val propertyTypeHint = _beanType match {
      case tt: TupleType => Some(tt.arguments(_tag - 1))
      case _ => 
        for {
          member <- field orElse getter
          classInfo <- ScalaTypeCompiler.classInfoOf(_beanType)
          typeHint <- classInfo.findPropertyType(member.getName)
        } yield typeHint
    }

    ((field, getter, setter): @unchecked) match {
      case (Some(f), None, None) =>
        if (Modifier.isFinal(f.getModifiers)) immutableFieldPropertyDescriptor(f, propertyTypeHint, ctorParameterIndex)
        else mutableFieldPropertyDescriptor(f, propertyTypeHint, ctorParameterIndex)

      case (Some(f), Some(g), None) =>
        getterPropertyDescriptor(g, propertyTypeHint, ctorParameterIndex)

      case (_, Some(g), Some(s)) =>
        getterSetterPropertyDescriptor(g, s, propertyTypeHint, ctorParameterIndex)
    }
  }
}