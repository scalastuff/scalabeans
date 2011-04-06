package org.scalabeans

import java.lang.reflect.{Modifier, Type}

object Preamble {
  implicit def toRichManifest(mf: Manifest[_]) = new {
    def isFinal = Modifier.isFinal(mf.erasure.getModifiers)
  }

  def descriptorOf[T <: AnyRef](implicit mf: Manifest[T]) = BeanIntrospector[T](mf)

  def descriptorOf(beanType: ScalaType) = BeanIntrospector[AnyRef](beanType)
  
  def scalaTypeOf[T](implicit mf: Manifest[T]) = ScalaType.scalaTypeOf(mf)

  def scalaTypeOf[T](t : Type) = ScalaType.scalaTypeOf(ManifestFactory.manifestOf(t))
}
