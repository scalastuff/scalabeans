package org.scalastuff.util

trait ForEach[M[_]] {
  def apply[A, B](m: M[A])(f: A => B): Unit
}

object ForEach {
  implicit object OptionForEach extends ForEach[Option] {
    def apply[A, B](m: Option[A])(f: A => B) = m.foreach(f)
  }

  implicit object TraversableForEach extends ForEach[Traversable] {
    def apply[A, B](m: Traversable[A])(f: A => B) = m.foreach(f)
  }

  implicit object ArrayForEach extends ForEach[Array] {
    def apply[A, B](m: Array[A])(f: A => B) = m.foreach(f)
  }
}