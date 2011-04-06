package org.scalabeans

import scala.collection.Seq

object Enum {
	def enumOf[C <: AnyRef](constantClass : Class[_]) : Option[Enum[C]] = try {
		val companionClass = constantClass.getClassLoader.loadClass(constantClass.getName + "$")
		if (classOf[Enum[C]].isAssignableFrom(companionClass)) {
			Some(companionClass.getField("MODULE$").get(null).asInstanceOf[Enum[C]])
		} else None
	} catch {
		case _ => None
	}
	def enumOf[C <: AnyRef](constant : C) : Option[Enum[C]] = enumOf[C](constant.getClass)
}

abstract class Enum[C <: AnyRef](implicit mf: Manifest[C]) {

	private var explicitOrdinals = Seq[(C,Int)]()
	private var explicitNames = Seq[(C,String)]()
	
	lazy val values : List[EnumValue[C]] = {
		var nextOrdinal = 1
		val values = getClass.getDeclaredFields.toList.
			filter (f => mf.erasure.isAssignableFrom(f.getType)).
			map { f =>
				f.setAccessible(true)
				val constant = f.get(Enum.this).asInstanceOf[C]
				val ordinal =  explicitOrdinals.find(_._1 eq constant).map(_._2).getOrElse {
					while (explicitOrdinals.exists(_._2 == nextOrdinal))
						nextOrdinal = nextOrdinal + 1
					nextOrdinal
				}
				nextOrdinal = ordinal + 1
				val name = explicitNames.find(_._1 eq constant).map(_._2).getOrElse(f.getName)
				EnumValue(Enum.this, name, ordinal, constant)
			}
    values.sortWith(_.ordinal < _.ordinal)
	}
	
	def valueOf(name : String) : Option[EnumValue[C]] = values.find(_.name == name)
	def valueOf(ordinal : Int) : Option[EnumValue[C]] = values.find(_.ordinal == ordinal)
	def valueOf(constant : C) : EnumValue[C] = values.find(_.constant == constant).get
	def nameOf(constant : C) : String = valueOf(constant).name
	def ordinalOf(constant : C) = valueOf(constant).ordinal
	override def toString = "Enum[" + mf.erasure.getSimpleName + "]"
	
	protected implicit def explicitOrdinal(constant : C) = new {
		def ordinal(ordinal : Int) = {
			explicitOrdinals :+= ((constant, ordinal))
			constant
		}
	}
	
	protected implicit def explicitName(constant : C) = new {
		def name(name : String) = {
			explicitNames :+= ((constant, name))
			constant
		}
	}
}

final case class EnumValue[C <: AnyRef] private[scalabeans] (enum : Enum[C], name : String, ordinal : Int, constant : C)
