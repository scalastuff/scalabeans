package org.scalastuff.proto

import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.proto.value.BeanValueHandler
import com.dyuproject.protostuff.Input

class BeanReader[B <: AnyRef](implicit mf: Manifest[B]) {
  private[this] val beanValueHandler = BeanValueHandler[B]()
  
  def readFrom(input: Input): B = beanValueHandler.beanReadFrom(input).asInstanceOf[B]
}