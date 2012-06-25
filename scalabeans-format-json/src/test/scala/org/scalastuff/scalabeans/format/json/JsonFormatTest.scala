package org.scalastuff.scalabeans.format.json

import org.scalatest.FlatSpec
import org.scalastuff.scalabeans.Preamble._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalastuff.scalabeans.testmono._
import org.scalastuff.util.Format

@RunWith(classOf[JUnitRunner])
class JsonFormatTest extends FlatSpec 
	with FormatBehaviours with ImmutableBeanTypesMonoBehaviours with MutableBeanTypesMonoBehaviours
	with CollectionsMonoBehaviours with RecursiveMetamodelBehaviours {
  
  def createMono[A <: AnyRef : Manifest]: Mono[A] = JsonFormatFactory.formatFor[A]

  import org.scalastuff.scalabeans.testmono.collectionproperties._
  
//  "JsonFormat" should "support immutable collection types" in {
//      checkInverse(new Person().adresses, new Person().set1().adresses)()
//    }

  "JsonFormat" should behave like immutableBeanTypesMono()
  
  it should behave like mutableBeanTypesMono()
  
  it should behave like inverseForCollections()
  
  it should behave like inverseForRecursiveMetamodels()

}