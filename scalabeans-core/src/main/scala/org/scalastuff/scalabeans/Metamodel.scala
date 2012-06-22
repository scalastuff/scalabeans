package org.scalastuff.scalabeans

import scala.reflect.ScalaSignature
import org.scalastuff.scalabeans.types._
import org.scalastuff.util.Converter
import Preamble._
import org.scalastuff.util.Functors
import org.scalastuff.util.Functor
import org.scalastuff.util.Rules

/**
 *  Represents information about 2 models: underlying and visible.
 *
 *  Underlying is the model used by application, visible is the model used by the interface.
 *  They are connected with each other and one can be derived from another.
 *
 *  While it is tempting to always use visible model, this is a good choice only in simple
 *  scenarios. Performance and integrity of references will be affected by excessive use of
 *  conversions. For this reasons it is better to stay with underlying model where possible and
 *  perform conversions only when you deal with ConvertedValueMetamodel.
 */
abstract class Metamodel { self =>
  val scalaType: ScalaType

  val visibleMetamodel: NotConvertedMetamodel
  
  val converter: Converter[Any, Any]

  def convert[A: Manifest, B: Manifest](_converter: Converter[A, B]): ConvertedMetamodel

  def convert(_converter: Converter[_, _], _visibleMetamodel: NotConvertedMetamodel): ConvertedMetamodel

  def toVisible(underlying: Any) = converter.to(underlying)
  def toUnderlying(visible: Any) = converter.from(visible)

  final def rewrite(rules: Rules[Metamodel]): Metamodel = {
    val result = rules(this)

    require(this.scalaType == result.scalaType,
      "Cannot rewrite: expect metamodel for type %s, found %s".format(this.scalaType, result.scalaType))

    result match {
      case LeafMetamodel(metamodel) => metamodel
      case _ => result.rewriteChildMetamodels(rules)
    }
  }

  def rewriteChildMetamodels(rules: Rules[Metamodel]): Metamodel

  override def toString() = "Metamodel(%s)".format(scalaType)
}

abstract class NotConvertedMetamodel extends Metamodel {
  final val visibleMetamodel = this
  final val converter = Converter.identity[Any]
  
  def convert[A: Manifest, B: Manifest](_converter: Converter[A, B]): ConvertedMetamodel = {
    require(scalaType == scalaTypeOf[A], "Source type of the converter doesn't match with the ScalaType of this metamodel")

    convert(_converter, metamodelOf[B])
  }

  def convert(_converter: Converter[_, _], _visibleMetamodel: NotConvertedMetamodel): ConvertedMetamodel = {
    new ConvertedValueMetamodel(
      scalaType,
      _converter.asInstanceOf[Converter[Any, Any]],
      _visibleMetamodel)
  }
}

/**
 * Represents metamodel of a value.
 *
 * Never contains conversions.
 */
final case class ValueMetamodel(val scalaType: ScalaType) extends NotConvertedMetamodel {
  def rewriteChildMetamodels(rules: Rules[Metamodel]) = this
}

/**
 * Must be used inside rewrite rules only, signals to rewriting strategy
 * to not rewrite childs of encapsulated metamodel.
 *
 * It is transparent to the client, ie rewrite(..) will never return it.
 */
final case class LeafMetamodel(val wrapped: Metamodel) extends NotConvertedMetamodel {
  /*
   * Actually subclassing Metamodel is a hack. It would be better to use union type Metamodel V Leaf
   * as result of Rules. Good idea for the future refactorings. 
   */
  val scalaType = wrapped.scalaType

  def rewriteChildMetamodels(rules: Rules[Metamodel]) = this
}

abstract class ConvertedMetamodel extends Metamodel { self =>
  def convert[A: Manifest, B: Manifest](_converter: Converter[A, B]) = {
    require(visibleMetamodel.scalaType == scalaTypeOf[A],
      "Cannot create ConvertedMetamodel: source type %s of the converter doesn't match with the visible ScalaType %s of this metamodel".
        format(scalaTypeOf[A], visibleMetamodel.scalaType))

    convert(_converter, metamodelOf[B])
  }

  def convert(_converter: Converter[_, _], _visibleMetamodel: NotConvertedMetamodel): ConvertedMetamodel = {
    new ConvertedValueMetamodel(
      scalaType,
      converter.compose(_converter.asInstanceOf[Converter[Any, Any]]),
      _visibleMetamodel)
  }
}

class ConvertedValueMetamodel private[scalabeans] (
  val scalaType: ScalaType,
  val converter: Converter[Any, Any],
  val visibleMetamodel: NotConvertedMetamodel) extends ConvertedMetamodel {

  def rewriteChildMetamodels(rules: Rules[Metamodel]) = (visibleMetamodel rewrite rules) match {
    case cv: ConvertedMetamodel =>
      convert(cv.converter, cv.visibleMetamodel)
    case mm: NotConvertedMetamodel => new ConvertedValueMetamodel(scalaType, converter, mm)
  }

  override def toString() = "ConvertedValueMetamodel(%s, %s)".format(scalaType, visibleMetamodel)
}

/**
 * Metamodel for homogeneous container types like Option, Array, Traversable.
 *
 * N.B. Metamodel of Tuple1 is a BeanDescriptor
 */
trait ContainerMetamodel[M[_]] extends Metamodel { self =>

  def elementMetamodel: Metamodel
  implicit val functor: Functor[M]
    
  require(scalaType.arguments.size == 1,
    "ScalaType of container must have only 1 argument (type parameter)")

  require(scalaType.arguments(0) == elementMetamodel.scalaType,
    "elementMetamodel doesn't match with the container type")

  def rewriteChildMetamodels(rules: Rules[Metamodel]) = elementMetamodel.rewrite(rules) match {
    case cv: ConvertedMetamodel =>
      new ContainerMetamodelWithConvertedElement[M](scalaType, cv)
    case mm @ _ =>
      new ContainerMetamodelImpl[M](scalaType, mm)
  }

  override def toString() = "ContainerMetamodel(%s, %s)".format(scalaType, elementMetamodel)
}

class ContainerMetamodelImpl[M[_]](
  val scalaType: ScalaType,
  val elementMetamodel: Metamodel)(implicit val functor: Functor[M]) 
  extends NotConvertedMetamodel with ContainerMetamodel[M]

object ContainerMetamodel {
  def unapply[M[_]](cm: ContainerMetamodel[M]) = Some(cm.elementMetamodel)
}

/**
 * Metamodel for homogeneous container types like Option, Array, Traversable
 * with element metamodel containing conversion.
 *
 *  Such models can be introspected in 2 ways: as converted container with visible type
 *  or as underlying container with converted values. Second option will in general be more
 *  efficient.
 */
class ContainerMetamodelWithConvertedElement[M[_]](
  val scalaType: ScalaType,
  val elementMetamodel: ConvertedMetamodel)(implicit val functor: Functor[M])
  extends ConvertedMetamodel with ContainerMetamodel[M]{

  /**
   * Provides conversion of the container to/from visible type using fmap and element converter.
   */
  val converter = new Converter[M[_], M[_]] {
    def to(underlying: M[_]) = functor.fmap(underlying, (elementMetamodel.toVisible _))
    def from(visible: M[_]) = functor.fmap(visible, (elementMetamodel.toUnderlying _))
  }.asInstanceOf[Converter[Any, Any]]

  val visibleMetamodel = metamodelOf(ScalaType(scalaType.erasure, Seq(elementMetamodel.visibleMetamodel.scalaType)))
}

object Metamodel {

  import Functors._

  def apply[A: Manifest](): NotConvertedMetamodel = apply(scalaTypeOf[A])
  def apply(scalaType: ScalaType): NotConvertedMetamodel = (scalaType match {
    case OptionType(elementType) => new ContainerMetamodelImpl[Option](scalaType, apply(elementType))
    case ArrayType(elementType) => new ContainerMetamodelImpl[Array](scalaType, apply(elementType))
    case TraversableType(elementType) => new ContainerMetamodelImpl[Traversable](scalaType, apply(elementType))
    case BeanType() => BeanDescriptor(scalaType)
    case _ => ValueMetamodel(scalaType)
  })

  def unapply(metamodel: Metamodel) = Some(metamodel.visibleMetamodel.scalaType)
}

trait Metamodels {
  def TupleBeanDescriptor(propertyMetamodel: Metamodel) = {
    val bd = descriptorOf(TupleType(propertyMetamodel.scalaType))
    bd rewriteProperties {
      case p @ PropertyDescriptor("_1", _) => p.updateMetamodel(propertyMetamodel)
    }
  }

  def TupleBeanDescriptor(keyMetamodel: Metamodel, valueMetamodel: Metamodel) = {
    val bd = descriptorOf(TupleType(keyMetamodel.scalaType, valueMetamodel.scalaType))
    bd rewriteProperties {
      case p @ PropertyDescriptor("_1", _) => p.updateMetamodel(keyMetamodel)
      case p @ PropertyDescriptor("_2", _) => p.updateMetamodel(valueMetamodel)
    }
  }
  
  def OptionMetamodel(elementMetamodel: Metamodel) = {
    val cm = metamodelOf(OptionType(elementMetamodel.scalaType))
    cm rewrite metamodelRules {
      case ContainerMetamodel(_) => LeafMetamodel(elementMetamodel)
    }
  }
}