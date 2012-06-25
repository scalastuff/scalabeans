package org.scalastuff.scalabeans.format.json

object Whatever extends App {

  import testbeans._

  val fmt = JsonFormatFactory.formatFor[TestBean]

  val ser = fmt.toString(TestBean(null))
  println(ser)
  
  val deser = fmt.readFrom(ser)
  println(deser)
  
  trait Whatever {
    def f: String
  }
  
  trait OverrideWharever extends Whatever {
    abstract override def f = "x=" + super.f 
  }
  
  sealed class WhateverImpl extends Whatever {
    def f = "whatever"
  } 
  object WhateverImpl extends WhateverImpl with OverrideWharever
  
  println(WhateverImpl.f)
}

package testbeans {
  case class TestBean(s: String)
}