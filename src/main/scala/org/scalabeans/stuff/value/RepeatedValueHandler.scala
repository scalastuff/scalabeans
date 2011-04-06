package org.scalabeans.stuff.value

import scala.collection.mutable.Builder
import com.dyuproject.protostuff.Input
import com.dyuproject.protostuff.Output
import com.dyuproject.protostuff.Pipe

abstract class RepeatedValueHandler(val elementValueHandler: ValueHandler) extends ValueHandler {
  type V = Traversable[elementValueHandler.V]
  type CB = Builder[elementValueHandler.V, V]

  override val inlined = true

  def defaultValue = {
    val builder = newBuilder()
    builder.result()
  }
  override def isDefaultValue(v: V) = v.isEmpty
  
  def readFrom(input: Input) = throw new UnsupportedOperationException("Cannot deserialize collection of collection, use wrapper")

  def readElementFrom(input: Input) = elementValueHandler.readFrom(input)

  def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = {
    require(!repeated, "Collection cannot be serialized inside another collection directly, use wrapper")
    value foreach (elementValueHandler.writeValueTo(tag, output, _, true))
  }

  def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
	  elementValueHandler.transfer(tag, pipe, input, output, repeated)
  }

  def newBuilder(): CB
}