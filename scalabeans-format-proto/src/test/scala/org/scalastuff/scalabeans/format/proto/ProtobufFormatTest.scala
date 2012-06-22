package org.scalastuff.scalabeans.format.proto

import org.scalatest.FlatSpec
import org.scalastuff.scalabeans.Preamble._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalastuff.scalabeans.testmono._
import org.scalastuff.util.Format

@RunWith(classOf[JUnitRunner])
class ProtobufFormatTest extends FlatSpec with ObjectGraphMonoBehaviours 
	with FormatBehaviours with ImmutableBeanTypesMonoBehaviours with MutableBeanTypesMonoBehaviours
	with CollectionsMonoBehaviours with RecursiveMetamodelBehaviours {
  
  def createMono[A <: AnyRef : Manifest]: Mono[A] = GraphProtostuffFormatFactory().formatFor[A]
    
  "ProtobufFormat" should behave like immutableBeanTypesMono()
  
  it should behave like mutableBeanTypesMono()
  
  it should behave like inverseForCollections()
  
  it should behave like inverseForRecursiveMetamodels()
}