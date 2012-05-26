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

import reflect.Manifest
import java.lang.reflect.{ TypeVariable, WildcardType, ParameterizedType, Type, GenericArrayType }
import org.scalastuff.scalabeans.types.{ScalaType, MapType}

object ManifestFactory {
  def manifestOf(t: Type): Manifest[_] = t match {
    case c: Class[_] => fromClass(c)

    case pt: ParameterizedType =>
      val clazz = manifestOf(pt.getRawType).erasure
      val typeArgs = pt.getActualTypeArguments map manifestOf

      if (pt.getOwnerType == null) {
        manifestOf(clazz, typeArgs)
      } else {
        Manifest.classType(manifestOf(pt.getOwnerType), clazz, typeArgs: _*)
      }

    case at: GenericArrayType =>
      val componentManifest = manifestOf(at.getGenericComponentType)
      val arrayManifest = componentManifest.arrayManifest // strips component type args off
      Manifest.classType(arrayManifest.erasure, componentManifest)

    case wt: WildcardType =>
      val upper = wt.getUpperBounds
      if (upper != null && upper.size > 0) manifestOf(upper(0))
      else manifestOf(classOf[AnyRef])

    case wt: TypeVariable[_] =>
      val upper = wt.getBounds
      if (upper != null && upper.size > 0) manifestOf(upper(0))
      else manifestOf(classOf[AnyRef])
  }

  def manifestOf(erasure: Class[_], typeArgs: Seq[Manifest[_]]): Manifest[_] = {
    if (typeArgs.size == 0) {
      fromClass(erasure)
    } else {
      val normalizedErasure =
        if (erasure.getName == "scala.Array")
          typeArgs(0).arrayManifest.erasure
        else
          erasure

      Manifest.classType(normalizedErasure, typeArgs.head, typeArgs.tail: _*)
    }
  }

  def manifestOf(st: ScalaType): Manifest[_] = st match {
    case MapType(keyType, valueType) =>
      manifestOf(st.erasure, Seq(manifestOf(keyType), manifestOf(valueType)))
    case _ =>
      val typeArgs = st.arguments map manifestOf
      manifestOf(st.erasure, typeArgs)
  }

  private def fromClass(clazz: Predef.Class[_]): Manifest[_] = clazz match {
    case java.lang.Byte.TYPE => Manifest.Byte
    case java.lang.Short.TYPE => Manifest.Short
    case java.lang.Character.TYPE => Manifest.Char
    case java.lang.Integer.TYPE => Manifest.Int
    case java.lang.Long.TYPE => Manifest.Long
    case java.lang.Float.TYPE => Manifest.Float
    case java.lang.Double.TYPE => Manifest.Double
    case java.lang.Boolean.TYPE => Manifest.Boolean
    case java.lang.Void.TYPE => Manifest.Unit
    case _ => Manifest.classType(clazz)
  }
}