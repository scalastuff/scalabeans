/*
 * Copyright (c) 2011 ScalaStuff.org (joint venture of Alexander Dvorkovyy and Ruud Diterwich)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package org.scalastuff.scalabeans

object Enum {
	def enumOf[V <: AnyRef](valueClass : Class[_]) : Option[Enum[V]] = try {
		val companionClass = valueClass.getClassLoader.loadClass(valueClass.getName + "$")
		if (classOf[Enum[V]].isAssignableFrom(companionClass)) {
			Some(companionClass.getField("MODULE$").get(null).asInstanceOf[Enum[V]])
		} else None
	} catch {
		case _ => None
	}
	def enumOf[C <: AnyRef](constant : C) : Option[Enum[C]] = enumOf[C](constant.getClass)
}

abstract class Enum[V <: AnyRef](implicit mf: Manifest[V]) {

	private var explicitOrdinals = Seq[(V,Int)]()
	private var explicitNames = Seq[(V,String)]()
	
	lazy val values : Seq[V] = wrappers.map(_.value)
	lazy val names : Seq[String] = wrappers.map(_.name)
	lazy val ordinals : Seq[Int] = wrappers.map(_.ordinal)
	lazy val valuesByName : Map[String, V] = wrappers.map(w => (w.name,w.value)).toMap
	lazy val valuesByOrdinal : Map[Int, V] = wrappers.map(w => (w.ordinal,w.value)).toMap
	lazy val namesAndOrdinals : Map[V, (String,Int)] = wrappers.map(w => (w.value, (w.name, w.ordinal))).toMap
	
	private lazy val wrappers : Seq[EnumValueWrapper[V]] = {
		var nextOrdinal = 1
		val values = getClass.getDeclaredFields.toSeq.
			filter (f => mf.erasure.isAssignableFrom(f.getType)).
			map { f =>
				f.setAccessible(true)
				val constant = f.get(Enum.this).asInstanceOf[V]
				val ordinal =  explicitOrdinals.find(_._1 eq constant).map(_._2).getOrElse {
					while (explicitOrdinals.exists(_._2 == nextOrdinal))
						nextOrdinal = nextOrdinal + 1
					nextOrdinal
				}
				nextOrdinal = ordinal + 1
				val name = explicitNames.find(_._1 eq constant).map(_._2).getOrElse(f.getName)
				EnumValueWrapper(Enum.this, name, ordinal, constant)
			}
    values.sortWith(_.ordinal < _.ordinal)
	}
	
	def valueOf(name : String) : Option[V] = wrappers.find(_.name == name).map(_.value)
	def valueOf(ordinal : Int) : Option[V] = wrappers.find(_.ordinal == ordinal).map(_.value)
	def nameOf(ordinal : Int) : Option[String] = wrappers.find(_.ordinal == ordinal).map(_.name)
	def nameOf(value : V) : String = wrappers.find(_.value == value).map(_.name).getOrElse(throw new EnumException(value, this))
	def ordinalOf(name : String) : Option[Int] = wrappers.find(_.name == name).map(_.ordinal)
	def ordinalOf(value : V) : Int =  wrappers.find(_.value == value).map(_.ordinal).getOrElse(throw new EnumException(value, this))
	
	override def toString = "Enum[" + mf.erasure.getSimpleName + "]"
	
	protected implicit def explicitOrdinal(constant : V) = new {
		def ordinal(ordinal : Int) = {
			explicitOrdinals :+= ((constant, ordinal))
			constant
		}
	}
	
	protected implicit def explicitName(constant : V) = new {
		def name(name : String) = {
			explicitNames :+= ((constant, name))
			constant
		}
	}
}

private final case class EnumValueWrapper[V <: AnyRef] private[scalabeans] (enum : Enum[V], name : String, ordinal : Int, value : V)

class EnumException(value : AnyRef, enum : Enum[_]) extends RuntimeException("'%s' is not a value of enum '%s'".format(value, enum))
