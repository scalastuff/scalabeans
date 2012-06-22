package org.scalastuff.util

/**
 * Rules is a function of type A => A used to rewrite a structure to itself. 
 */
trait Rules[A] {
  def isDefinedAt(a: A): Boolean
  def apply(a: A): A
}

object Rules {
  def apply[A](pf: PartialFunction[A, A]) = new Rules[A] {
    def isDefinedAt(a: A) = pf.isDefinedAt(a)
    def apply(a: A) =
      if (isDefinedAt(a)) pf(a)
      else a
  }
}