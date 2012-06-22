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

import java.lang.reflect.{ Constructor, Field, Method, Modifier }
import com.thoughtworks.paranamer.BytecodeReadingParanamer
import collection.JavaConversions._
import Preamble._
import org.scalastuff.scalabeans.types.ScalaType

private[scalabeans] object BeanIntrospector {
//  def isBeanClass(clazz: Class[_]): Boolean = {
//    !clazz.getConstructors().isEmpty //&& !introspectProperties(clazz).isEmpty
//  }

  def print(c: Class[_], prefix: String = ""): Unit = {
    def static(mods: Int) = if (Modifier.isStatic(mods)) " (static)" else ""
    println(prefix + "Class: " + c.getName)
    println(prefix + "  Fields: ")
    for (f <- c.getDeclaredFields) {
      println(prefix + "    " + f.getName + " : " + f.getGenericType + static(f.getModifiers))
      if (f.getName == "$outer") print(f.getType, "      ")
    }
    println(prefix + "  Methods: ")
    for (f <- c.getDeclaredMethods) {
      println(prefix + "    " + f.getName + " : " + f.getGenericReturnType + static(f.getModifiers))
    }
    println(prefix + "  Sub classes: ")
    for (f <- c.getDeclaredClasses) {
      println(prefix + "    " + f.getName + static(f.getModifiers))
    }
    println(prefix + "  Enum Values: ")
    for (f <- c.getMethods filter (m => m.getParameterTypes.isEmpty && classOf[Enumeration$Value].isAssignableFrom(m.getReturnType))) {
      val instance = new Enumeration {}
      println(prefix + "    a")
    }
  }

  def introspectProperties(scalaType: ScalaType) = {
    def classExtent(c: Class[_]): List[Class[_]] = {
      if(c == null) Nil
      else if (c == classOf[AnyRef]) Nil
      else classExtent(c.getSuperclass) :+ c
    }

    /**
     * Searches for the method with given name in the given class. Overridden method discovered if present.
     */
    def findMethod(c: Class[_], name: String): Option[Method] = {
      if (c == null) None
      else if (c == classOf[AnyRef]) None
      else c.getDeclaredMethods.find(_.getName == name) orElse findMethod(c.getSuperclass(), name)
    }

    def typeSupported(scalaType: ScalaType) = {
      true // TODO: list of supported Java and Scala types ...
    }

    val c = scalaType.erasure

    var tag = 0 // tag is 1-based property index, see code below

    val fieldProperties = for {
      c <- (classExtent(c) toSeq)
      field <- c.getDeclaredFields
      name = field.getName

      if !name.contains('$')
      if !field.isSynthetic
      //      if typeSupported(scalaTypeOf(field.getGenericType))

      getter = findMethod(c, name)
      setter = findMethod(c, name + "_$eq")
    } yield {
      tag = tag + 1
      PropertyDescriptor.Model(scalaType, tag, Some(field), getter, setter)
    }

    val methodProperties = for {
      c <- (classExtent(c) toSeq)
      getter <- c.getDeclaredMethods
      name = getter.getName

      if getter.getParameterTypes.length == 0
      if getter.getReturnType != Void.TYPE
      if !name.contains('$')
      //      if typeSupported(scalaTypeOf(getter.getGenericReturnType))
      if !fieldProperties.exists(_.name == name)
      setter <- c.getDeclaredMethods.find(_.getName == name + "_$eq")

    } yield {
      tag = tag + 1
      PropertyDescriptor.Model(scalaType, tag, None, Some(getter), Some(setter))
    }

    fieldProperties ++ methodProperties
  }
}