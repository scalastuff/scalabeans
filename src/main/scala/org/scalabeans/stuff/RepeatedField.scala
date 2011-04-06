package org.scalabeans.stuff

import com.dyuproject.protostuff.Input
import org.scalabeans.PropertyDescriptor
import value.RepeatedValueHandler

trait RepeatedField[B <: AnyRef] extends MutableField[B] {

  override val valueHandler: RepeatedValueHandler

  override val repeated = true

  def newBuilder() = new Builder()

  class Builder {
    private[this] val builder = valueHandler.newBuilder()

    def tag = RepeatedField.this.tag

    def mergeElementFrom(input: Input) {
      val elem = valueHandler.readElementFrom(input)
      builder += elem
    }

    def sinkTo(obj: B) {
     RepeatedField.this.setValue(obj, builder.result())
    }
  }

}

