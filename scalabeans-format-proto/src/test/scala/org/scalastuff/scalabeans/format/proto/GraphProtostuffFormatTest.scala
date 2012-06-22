package org.scalastuff.scalabeans.format.proto

import org.scalatest.FlatSpec
import org.scalastuff.scalabeans.Preamble._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalastuff.scalabeans.testmono._
import org.scalastuff.util.Format

@RunWith(classOf[JUnitRunner])
class GraphProtostuffFormatTest extends FlatSpec with ObjectGraphMonoBehaviours 
	with FormatBehaviours with ImmutableBeanTypesMonoBehaviours with MutableBeanTypesMonoBehaviours
	with CollectionsMonoBehaviours with RecursiveMetamodelBehaviours {
  
  def createMono[A <: AnyRef : Manifest]: Mono[A] = GraphProtostuffFormatFactory().formatFor[A]
    
  "GraphProtostuffFormat" should behave like objectGraphMono()
  
  it should behave like immutableBeanTypesMono()
  
  it should behave like mutableBeanTypesMono()
  
  it should behave like inverseForCollections()
  
  it should behave like inverseForRecursiveMetamodels()
}