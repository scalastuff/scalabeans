package org.scalastuff.proto

import org.scalatest.FlatSpec
import org.scalastuff.scalabeans.format.proto.GraphProtostuffFormatFactory
import org.scalastuff.scalabeans.Preamble._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalastuff.scalabeans._
import org.scalastuff.util.Format

@RunWith(classOf[JUnitRunner])
class GraphProtostuffFormatTest extends FlatSpec with ObjectGraphConverterBehaviours 
	with FormatBehaviours with ImmutableBeanTypesMonoBehaviours with MutableBeanTypesMonoBehaviours {
  
  def createMono[A <: AnyRef : Manifest]: Mono[A] = GraphProtostuffFormatFactory().formatFor[A]
    
  "GraphProtostuffFormat" should behave like objectGraphConverter(GraphProtostuffFormatFactory().formatFor[ObjectGraph])
  
  it should behave like immutableBeanTypesMono()
  
  it should behave like mutableBeanTypesMono()
}