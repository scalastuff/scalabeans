package org.scalastuff.util

trait Functor[M[_]] {
  def fmap[A, B](m: M[A], f: A => B): M[B]
}

object Functors {
  implicit object OptionFunctor extends Functor[Option] {
    def fmap[A, B](m: Option[A], f: A => B) = m map f
  }

  implicit object TraversableFunctor extends Functor[Traversable] {
    def fmap[A, B](m: Traversable[A], f: A => B) = m map f
  }

  implicit object ArrayFunctor extends Functor[Array] {
    def fmap[A, B](m: Array[A], f: A => B) = (m map f).array.asInstanceOf[Array[B]]
  }
}