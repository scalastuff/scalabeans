package org.scalastuff.scalabeans.sig

import org.junit.Test
import org.junit.Assert._
import Mirror._

class ClassDeclExtractorTest {
  @Test
  def testSimplePropertiesDetection() {
    val top = ClassDeclExtractor.extract[testbeans.SimplePropertiesTest].get
    val classes = top.collect { case cd: ClassDecl => cd }

    assertEquals(1, classes.size)
    val classDecl = classes(0)
    println(classDecl)

    assertEquals(7, classDecl.values.size)
    checkProp(classDecl.values(0), "ai", "scala.Array[scala.Int]")
    checkProp(classDecl.values(1), "aoi", "scala.Array[scala.Option[scala.Int]]")
    checkProp(classDecl.values(2), "aoli", "scala.Array[scala.Option[scala.package.type#List[scala.Int]]]")
    checkProp(classDecl.values(3), "opt", "scala.Option[scala.Int]")
    checkProp(classDecl.values(4), "listopt", "scala.package.type#List[scala.Option[scala.Int]]")
    checkProp(classDecl.values(5), "map", "scala.Predef.type#Map[scala.Option[scala.Predef.type#String], scala.package.type#List[scala.Option[scala.Float]]]")
    checkProp(classDecl.values(6), "priv", "scala.package.type#Seq[scala.Boolean]")
  }

  def checkProp(valDecl: ValueDecl, name: String, typeStr: String) {
    assertEquals(name, valDecl.name)
    assertEquals(typeStr, valDecl.valueType.toString)
  }

  //  def printSymbolTree(entry: SymbolDeclEntry, level: Int, children: Map[SymbolDeclEntry, ArrayBuffer[SymbolDeclEntry]])(implicit table: Array[Entry]) {
  //    print(" " * level)
  //    println(entry.name + ": " + entry)
  //    for {
  //      scopeChildren <- children.get(entry)
  //      child <- scopeChildren
  //    } printSymbolTree(child, level + 1, children)
  //  }
}

package testbeans {
  class SimplePropertiesTest {
    val ai: Array[Int] = null
    val aoi: Array[Option[Int]] = null
    val aoli: Array[Option[List[Int]]] = null
    val opt: Option[Int] = None
    val listopt: List[Option[Int]] = Nil
    var map: Map[Option[String], List[Option[Float]]] = Map.empty

    // this is function, not property
    def funct(x: Int, y: Option[Int]): Int = x

    private[this] val priv: Seq[Boolean] = Seq.empty
  }
  
  abstract class SuperTestBean[A, E](lista: List[A], val vala: A, var vara: A, arra: Array[A]) {
    type B
    type C = String
    
    val seqb: Seq[B] = Seq.empty
    // val arrayb: Array[B] = Array.empty[B] // doesn't compile
    val arrayc: Array[C] = Array.empty[C]
    def meth1[D](d: D): Option[D]
  }
  
  class SigTestBean[A](lista: List[A], vala: A, vara: A, arra: Array[A]) extends SuperTestBean[A, Int](lista, vala, vara, arra) {
    type B = Long
    
    def meth1[D](d: D): Option[D] = Option(d)
    
    def meth2[D <: C](d: D): Option[D] = Option(d)
  }
}