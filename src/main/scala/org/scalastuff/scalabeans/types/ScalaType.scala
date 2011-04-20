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
package types

import scala.reflect.Manifest
import collection.mutable.{Builder, ArrayBuilder}
import collection.generic.{MapFactory, GenericCompanion}

trait ScalaType {
  def erasure: Class[_]

  def arguments: Seq[ScalaType]

  override def equals(other: Any) = other match {
    case that: ScalaType => erasure == that.erasure && arguments == that.arguments
    case _ => false
  }

  override def hashCode = 41 * (41 + erasure.hashCode) + arguments.hashCode

  override def toString = erasure.getSimpleName + (if (arguments.isEmpty) "" else arguments.mkString("[", ",", "]"))
}

private[types] trait SingleArgument extends ScalaType {
  def argument = arguments.head
}

// ******* AnyVal ******

trait AnyValType extends ScalaType

object AnyValType {
  def unapply(t: AnyValType) = true
}

case object IntType extends Impl(java.lang.Integer.TYPE) with AnyValType

case object LongType extends Impl(java.lang.Long.TYPE) with AnyValType

case object FloatType extends Impl(java.lang.Float.TYPE) with AnyValType

case object DoubleType extends Impl(java.lang.Double.TYPE) with AnyValType

case object BooleanType extends Impl(java.lang.Boolean.TYPE) with AnyValType

case object CharType extends Impl(java.lang.Character.TYPE) with AnyValType

case object ShortType extends Impl(java.lang.Short.TYPE) with AnyValType

case object ByteType extends Impl(java.lang.Byte.TYPE) with AnyValType

// ******* AnyRef ******

trait AnyRefType extends ScalaType

object AnyRefType {
  def unapply(t: AnyRefType) = true
}

trait OptionType extends AnyRefType with SingleArgument

object OptionType {
  def unapply(t: OptionType) = Some(t.argument)
}

// allow "case StringType =>" and "case StringType() =>" syntax
class StringType extends Impl(classOf[String])

object StringType extends StringType {
  def unapply(t: StringType) = true
}

class BigDecimalType extends Impl(classOf[BigDecimal])

object BigDecimalType extends BigDecimalType {
  def unapply(t: BigDecimalType) = true
}

class BigIntType extends Impl(classOf[BigInt])

object BigIntType extends BigIntType {
  def unapply(t: BigIntType) = true
}

class DateType extends Impl(classOf[java.util.Date])

object DateType extends DateType {
  def unapply(t: DateType) = true
}

/**
 * Is not subclass of DateType, since Timestamp instance cannot be used as java.util.Date
 */
class SqlTimestampType extends Impl(classOf[java.sql.Timestamp])

/**
 * Is not subclass of DateType, since Timestamp instance cannot be used as java.util.Date
 */
object SqlTimestampType extends SqlTimestampType {
  def unapply(t: SqlTimestampType) = true
}

/**
 * Is not subclass of DateType, since java.sql.Date instance cannot be used as java.util.Date
 */
class SqlDateType extends Impl(classOf[java.sql.Date])

/**
 * Is not subclass of DateType, since java.sql.Date instance cannot be used as java.util.Date
 */
object SqlDateType extends SqlDateType {
  def unapply(t: SqlDateType) = true
}

trait TupleType extends AnyRefType

object TupleType {
  def apply(arg1: ScalaType) = new Impl(classOf[Tuple1[_]], arg1) with TupleType
  def apply(arg1: ScalaType, arg2: ScalaType) = new Impl(classOf[Tuple2[_, _]], arg1, arg2) with TupleType
  def unapply(t: TupleType) = Some((t.arguments(0), t.arguments(1)))
}

// ******* Enums ******

trait EnumType extends AnyRefType {
	def enum : Enum[AnyRef]
	
  override def equals(other: Any) = other match {
    case that: EnumType => super.equals(other) && enum == that.enum
    case _ => false
  }
	override def hashCode = 41 * (41 + super.hashCode) + enum.hashCode
}

object EnumType {
	def unapply(t: EnumType) = Some(t.enum)
}

// ******* Arrays ******
trait ArrayType extends AnyRefType with SingleArgument {
  override def toString() = "Array[" + argument.toString + "]"
  def newArrayBuilder[A](): ArrayBuilder[A]
}

object ArrayType {
	def unapply(t: ArrayType) = Some(t.argument)
}

// ******* Collections ******

trait CollectionType extends AnyRefType with SingleArgument {
  val newBuilder: Option[() => Builder[Any, Traversable[Any]]] = {
    try {
      val compField = Class.forName(erasure.getName + "$").getDeclaredField("MODULE$")
      compField.setAccessible(true)
      val companion = compField.get(null)
      companion match {
        case gc: GenericCompanion[Traversable] => Some({() => gc.newBuilder[Any] })
        case mapFactory: MapFactory[Map] => Some({() => mapFactory.newBuilder[Any, Any].asInstanceOf[Builder[Any, Traversable[Any]]] })
        case _ => None
      }
    } catch {
      case e: Exception =>
        println(e)
        None
    }
  }

  //def newBuilder = companion map (_.newBuilder)
}

object CollectionType {
  def unapply(t: CollectionType) = Some(t.argument)
}

trait ImmutableCollectionType extends CollectionType

object ImmutableCollectionType {
  def unapply(t: ImmutableCollectionType) = Some(t.argument)
}

trait MutableCollectionType extends CollectionType

object MutableCollectionType {
  def unapply(t: MutableCollectionType) = Some(t.argument)
}

trait TraversableType extends CollectionType

object TraversableType {
  def unapply(t: TraversableType) = Some(t.argument)
}

trait IterableType extends TraversableType

object IterableType {
  def unapply(t: IterableType) = Some(t.argument)
}

trait SeqType extends IterableType

object SeqType {
  def unapply(t: SeqType) = Some(t.argument)
}

trait ImmutableSeqType extends SeqType with ImmutableCollectionType

object ImmutableSeqType {
  def unapply(t: ImmutableSeqType) = Some(t.argument)
}

trait MutableSeqType extends SeqType with MutableCollectionType

object MutableSeqType {
  def unapply(t: MutableSeqType) = Some(t.argument)
}

trait IndexedSeqType extends SeqType

object IndexedSeqType {
  def unapply(t: IndexedSeqType) = Some(t.argument)
}

trait ImmutableIndexedSeqType extends IndexedSeqType with ImmutableSeqType

object ImmutableIndexedSeqType {
  def unapply(t: ImmutableIndexedSeqType) = Some(t.argument)
}

trait MutableIndexedSeqType extends IndexedSeqType with MutableSeqType

object MutableIndexedSeqType {
  def unapply(t: MutableIndexedSeqType) = Some(t.argument)
}

trait VectorType extends ImmutableIndexedSeqType

object VectorType {
  def unapply(t: VectorType) = Some(t.argument)
}

trait ResizableArrayType extends MutableIndexedSeqType

object ResizableArrayType {
  def unapply(t: ResizableArrayType) = Some(t.argument)
}

trait LinearSeqType extends SeqType

object LinearSeqType {
  def unapply(t: LinearSeqType) = Some(t.argument)
}

trait ImmutableLinearSeqType extends LinearSeqType with ImmutableSeqType

object ImmutableLinearSeqType {
  def unapply(t: ImmutableLinearSeqType) = Some(t.argument)
}

trait MutableLinearSeqType extends LinearSeqType with MutableSeqType

object MutableLinearSeqType {
  def unapply(t: MutableLinearSeqType) = Some(t.argument)
}

trait ListType extends ImmutableLinearSeqType

object ListType {
  def unapply(t: ListType) = Some(t.argument)
}

trait StreamType extends ImmutableLinearSeqType

object StreamType {
  def unapply(t: StreamType) = Some(t.argument)
}

trait BufferType extends MutableSeqType

object BufferType {
  def unapply(t: BufferType) = Some(t.argument)
}

trait ListBufferType extends BufferType

object ListBufferType {
  def unapply(t: ListBufferType) = Some(t.argument)
}

trait ArrayBufferType extends BufferType

object ArrayBufferType {
  def unapply(t: ArrayBufferType) = Some(t.argument)
}

trait MutableListType extends MutableLinearSeqType

object MutableListType {
  def unapply(t: MutableListType) = Some(t.argument)
}

trait SetType extends IterableType

object SetType {
  def unapply(t: SetType) = Some(t.argument)
}

trait ImmutableSetType extends SetType with ImmutableCollectionType

object ImmutableSetType {
  def unapply(t: ImmutableSetType) = Some(t.argument)
}

trait MutableSetType extends SetType with MutableCollectionType

object MutableSetType {
  def unapply(t: MutableSetType) = Some(t.argument)
}

trait SortedSetType extends ImmutableSetType

object SortedSetType {
  def unapply(t: SortedSetType) = Some(t.argument)
}

trait TreeSetType extends SortedSetType

object TreeSetType {
  def unapply(t: TreeSetType) = Some(t.argument)
}

trait HashSetType extends SetType

object HashSetType {
  def unapply(t: HashSetType) = Some(t.argument)
}

trait ImmutableHashSetType extends ImmutableSetType with HashSetType

object ImmutableHashSetType {
  def unapply(t: ImmutableHashSetType) = Some(t.argument)
}

trait MutableHashSetType extends MutableSetType with HashSetType

object MutableHashSetType {
  def unapply(t: MutableHashSetType) = Some(t.argument)
}

trait LinkedHashSetType extends MutableHashSetType

object LinkedHashSetType {
  def unapply(t: LinkedHashSetType) = Some(t.argument)
}

trait MapType extends IterableType {
  def keyType = argument.arguments(0)

  def valueType = argument.arguments(1)
}

object MapType {
  def unapply(t: MapType) = Some((t.keyType, t.valueType))
}

trait ImmutableMapType extends MapType with ImmutableCollectionType

object ImmutableMapType {
  def unapply(t: ImmutableMapType) = Some((t.keyType, t.valueType))
}

trait MutableMapType extends MapType with MutableCollectionType

object MutableMapType {
  def unapply(t: MutableMapType) = Some((t.keyType, t.valueType))
}

trait SortedMapType extends ImmutableMapType

object SortedMapType {
  def unapply(t: SortedMapType) = Some((t.keyType, t.valueType))
}

trait TreeMapType extends SortedMapType

object TreeMapType {
  def unapply(t: TreeMapType) = Some((t.keyType, t.valueType))
}

trait HashMapType extends MapType

object HashMapType {
  def unapply(t: HashMapType) = Some((t.keyType, t.valueType))
}

trait ImmutableHashMapType extends ImmutableMapType with HashMapType

object ImmutableHashMapType {
  def unapply(t: ImmutableHashMapType) = Some((t.keyType, t.valueType))
}

trait MutableHashMapType extends MutableMapType with HashMapType

object MutableHashMapType {
  def unapply(t: MutableHashMapType) = Some((t.keyType, t.valueType))
}

trait LinkedHashMapType extends MutableHashMapType

object LinkedHashMapType {
  def unapply(t: LinkedHashMapType) = Some((t.keyType, t.valueType))
}

object ScalaType {

  private object TheAnyRefType extends Impl(classOf[AnyRef])

  def scalaTypeOf[A](implicit mf: Manifest[A]): ScalaType = {
    def arg(index: Int): ScalaType = if (index < mf.typeArguments.size) scalaTypeOf(mf.typeArguments(index)) else TheAnyRefType
    def argTuple2 = new Impl(classOf[Tuple2[_, _]], arg(0), arg(1)) with TupleType
    
    def createArrayType(mf: Manifest[_]) = {
	  
	  val contentTypeManifest =
      if (mf.typeArguments.size > 0) mf.typeArguments(0)
      else ManifestFactory.manifestOf(mf.erasure.getComponentType)

	  new Impl(mf.erasure, scalaTypeOf(contentTypeManifest)) with ArrayType {
      def newArrayBuilder[A](): ArrayBuilder[A] = contentTypeManifest.newArrayBuilder().asInstanceOf[ArrayBuilder[A]]
    }
	}
    
    if (classOf[Int] == mf.erasure || classOf[java.lang.Integer] == mf.erasure) IntType
    else if (classOf[Long] == mf.erasure || classOf[java.lang.Long] == mf.erasure) LongType
    else if (classOf[Float] == mf.erasure || classOf[java.lang.Float] == mf.erasure) FloatType
    else if (classOf[Double] == mf.erasure || classOf[java.lang.Double] == mf.erasure) DoubleType
    else if (classOf[Boolean] == mf.erasure || classOf[java.lang.Boolean] == mf.erasure) BooleanType
    else if (classOf[Char] == mf.erasure || classOf[java.lang.Character] == mf.erasure) CharType
    else if (classOf[Short] == mf.erasure || classOf[java.lang.Short] == mf.erasure) ShortType
    else if (classOf[Byte] == mf.erasure || classOf[java.lang.Byte] == mf.erasure) ByteType
    else if (classOf[String] == mf.erasure) StringType
    else if (classOf[BigDecimal] == mf.erasure) BigDecimalType
    else if (classOf[BigInt] == mf.erasure) BigIntType
    else if (classOf[java.util.Date] == mf.erasure) DateType
    else if (classOf[java.sql.Timestamp] == mf.erasure) SqlTimestampType
    else if (classOf[java.sql.Date] == mf.erasure) SqlDateType
    else if (classOf[scala.Option[_]] == mf.erasure) new Impl(mf.erasure, arg(0)) with OptionType
    else if (classOf[scala.Tuple1[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with TupleType
    else if (classOf[scala.Tuple2[_, _]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0), arg(1)) with TupleType
    else if (mf.erasure.isArray) createArrayType(mf)
    else if (classOf[scala.collection.mutable.LinkedHashMap[_, _]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, argTuple2) with LinkedHashMapType
    else if (classOf[scala.collection.immutable.HashMap[_, _]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, argTuple2) with ImmutableHashMapType
    else if (classOf[scala.collection.mutable.HashMap[_, _]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, argTuple2) with MutableHashMapType
    else if (classOf[scala.collection.immutable.TreeMap[_, _]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, argTuple2) with TreeMapType
    else if (classOf[scala.collection.immutable.SortedMap[_, _]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, argTuple2) with SortedMapType
    else if (classOf[scala.collection.immutable.Map[_, _]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, argTuple2) with ImmutableMapType
    else if (classOf[scala.collection.mutable.Map[_, _]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, argTuple2) with MutableMapType
    else if (classOf[scala.collection.Map[_, _]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, argTuple2) with MapType
    else if (classOf[scala.collection.mutable.LinkedHashSet[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with LinkedHashSetType
    else if (classOf[scala.collection.immutable.HashSet[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with ImmutableHashSetType
    else if (classOf[scala.collection.mutable.HashSet[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with MutableHashSetType
    else if (classOf[scala.collection.immutable.TreeSet[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with TreeSetType
    else if (classOf[scala.collection.immutable.SortedSet[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with SortedSetType
    else if (classOf[scala.collection.immutable.Set[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with ImmutableSetType
    else if (classOf[scala.collection.mutable.Set[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with MutableSetType
    else if (classOf[scala.collection.Set[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with SetType
    else if (classOf[scala.collection.mutable.ArrayBuffer[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with ArrayBufferType
    else if (classOf[scala.collection.mutable.ListBuffer[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with ListBufferType
    else if (classOf[scala.collection.mutable.Buffer[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with BufferType
    else if (classOf[scala.collection.immutable.Stream[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with StreamType
    else if (classOf[scala.collection.immutable.List[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with ListType
    else if (classOf[scala.collection.mutable.MutableList[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with MutableListType
    else if (classOf[scala.collection.immutable.LinearSeq[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with ImmutableLinearSeqType
    else if (classOf[scala.collection.mutable.LinearSeq[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with MutableLinearSeqType
    else if (classOf[scala.collection.LinearSeq[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with LinearSeqType
    else if (classOf[scala.collection.mutable.ResizableArray[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with ResizableArrayType
    else if (classOf[scala.collection.immutable.Vector[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with VectorType
    else if (classOf[scala.collection.immutable.IndexedSeq[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with ImmutableIndexedSeqType
    else if (classOf[scala.collection.mutable.IndexedSeq[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with MutableIndexedSeqType
    else if (classOf[scala.collection.IndexedSeq[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with IndexedSeqType
    else if (classOf[scala.collection.Seq[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with SeqType
    else if (classOf[scala.collection.Iterable[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with IterableType
    else if (classOf[scala.collection.Traversable[_]].isAssignableFrom(mf.erasure)) new Impl(mf.erasure, arg(0)) with TraversableType
    else Enum.enumOf[AnyRef](mf.erasure) match {
    	case Some(e) => new Impl(mf.erasure, mf.typeArguments.map(scalaTypeOf(_)): _*) with EnumType {
    		def enum = e
    	}
    	case None => new Impl(mf.erasure, mf.typeArguments.map(scalaTypeOf(_)): _*) with AnyRefType
    }
  }
}

private[scalabeans] abstract class Impl(val erasure: Class[_], val arguments: ScalaType*) extends ScalaType