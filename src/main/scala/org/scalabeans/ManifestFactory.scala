package org.scalabeans

import reflect.Manifest
import java.lang.reflect.{TypeVariable, WildcardType, ParameterizedType, Type}

object ManifestFactory {
  def manifestOf(t: Type): Manifest[_] = t match {
    case c: Class[_] => fromClass(c)

    case pt: ParameterizedType =>
      val clazz = manifestOf(pt.getRawType).erasure
      val typeArgs = pt.getActualTypeArguments map manifestOf

      if (pt.getOwnerType == null) {
        if (typeArgs.size == 0) {
          fromClass(clazz)
        } else {
          Manifest.classType(clazz, typeArgs.head, typeArgs.tail: _*)
        }
      } else {
        Manifest.classType(manifestOf(pt.getOwnerType), clazz, typeArgs: _*)
      }

    case wt: WildcardType =>
      val upper = wt.getUpperBounds
      if (upper != null && upper.size > 0) manifestOf(upper(0))
      else manifestOf(classOf[AnyRef])

    case wt: TypeVariable[_] =>
      val upper = wt.getBounds
      if (upper != null && upper.size > 0) manifestOf(upper(0))
      else manifestOf(classOf[AnyRef])
  }

  def manifestOf(st: ScalaType): Manifest[_] = {
    val typeArgs = st.arguments map manifestOf
    if (typeArgs.size == 0) {
      fromClass(st.erasure)
    } else {
      Manifest.classType(st.erasure, typeArgs.head, typeArgs.tail: _*)
    }
  }

  private def fromClass(clazz: Predef.Class[_]): Manifest[_] = clazz match {
    case java.lang.Byte.TYPE => Manifest.Byte.asInstanceOf[Manifest[_]]
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