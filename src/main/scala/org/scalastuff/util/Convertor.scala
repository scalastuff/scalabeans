package org.scalastuff.util

trait Convertor[A, B] {
  def to(a: A): B
  def from(b: B): A
  
  def compose[C](other: Convertor[B, C]) = new Convertor[A, C] { 
    def to(a: A) = other.to(Convertor.this.to(a))
    def from(c: C) = Convertor.this.from(other.from(c))
  }
}

object Convertor {
  def apply[A, B](_to: A => B, _from: B => A) = new Convertor[A, B] {
    def to(a: A) = _to(a)
    def from(b: B) = _from(b)
  }
  
  def identity[A] = new Convertor[A, A] {
    def to(a: A) = a
    def from(b: A) = b
  } 
  
  implicit def format2convertor[A](fmt: Format[A]) = new Convertor[A, Array[Byte]] {
    def to(a: A) = fmt.toByteArray(a)
    def from(b: Array[Byte]) = fmt.readFrom(b)
  }
}