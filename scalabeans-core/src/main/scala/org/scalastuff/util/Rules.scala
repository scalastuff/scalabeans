package org.scalastuff.util

/**
 * Rules is a function of type A => A used to rewrite a structure to itself. 
 */
trait Rules[A] {
  
  def apply(x: A): A
  
  /** 
   * Composes two instances of Rules[A] in a new Rules[A], with this rules applied last.
   */
  def compose(g: Rules[A]): Rules[A] = new Rules[A] {
    def apply(x: A) = Rules.this.apply(g(x)) 
  }

  /** 
   * Composes two instances of Rules[A] in a new Rules[A], with this rules applied first.
   */
  def andThen(g: Rules[A]): Rules[A] = new Rules[A] {
    def apply(x: A) = g(Rules.this.apply(x)) 
  }
}

object Rules {
  def apply[A](pf: PartialFunction[A, A]) = new Rules[A] {
    def apply(a: A) =
      if (pf.isDefinedAt(a)) pf(a)
      else a
  }
}