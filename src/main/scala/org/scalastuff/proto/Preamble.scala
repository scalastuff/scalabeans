package org.scalastuff.proto

object Preamble {
  def readerOf[B <: AnyRef](implicit mf: Manifest[B]) = new BeanReader[B]
  def writerOf[B <: AnyRef](implicit mf: Manifest[B]) = new BeanWriter[B]
}