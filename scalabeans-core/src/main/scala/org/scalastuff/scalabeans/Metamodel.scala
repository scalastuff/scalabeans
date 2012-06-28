package org.scalastuff.scalabeans

import scala.reflect.ScalaSignature
import org.scalastuff.scalabeans.types._
import org.scalastuff.scalabeans.converters.Converter
import Preamble._
import org.scalastuff.util.Rules
import scala.collection.mutable.Builder
import org.scalastuff.util.{ Functor, ForEach }
import org.scalastuff.scalabeans.converters.RuntimeConverter

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

  @inline final def convert[A: Manifest, B: Manifest](converter: Converter[A, B]): ConvertedMetamodel =
    convert(RuntimeConverter(scalaTypeOf[A], scalaTypeOf[B], converter))

  final def convert(runtimeConverter: RuntimeConverter): ConvertedMetamodel = {
    require(visibleMetamodel.scalaType == runtimeConverter.sourceType,
      "Cannot create ConvertedMetamodel: source type %s of the converter doesn't match with the visible ScalaType %s of this metamodel".
        format(runtimeConverter.sourceType, visibleMetamodel.scalaType))

    convert(runtimeConverter.converter.asInstanceOf[Converter[Any, Any]], metamodelOf(runtimeConverter.targetType))
  }

  protected def convert(converter: Converter[Any, Any], _visibleMetamodel: NotConvertedMetamodel): ConvertedMetamodel

  def toVisible(underlying: Any) = converter.to(underlying)
  def toUnderlying(visible: Any) = converter.from(visible)

  final def rewrite(rules: Rules[Metamodel]): Metamodel = {
    val result = rules(this)

    require(this.scalaType == result.scalaType,
      "Cannot rewrite: expect metamodel for type %s, found %s".format(this.scalaType, result.scalaType))

    result.rewriteChildMetamodels(rules)
  }

  def rewriteChildMetamodels(rules: Rules[Metamodel]): Metamodel

  override def toString() = "Metamodel(%s)".format(scalaType)
}

abstract class NotConvertedMetamodel extends Metamodel {
  final val visibleMetamodel = this
  final val converter = Converter.identity[Any]

  protected def convert(_converter: Converter[Any, Any], _visibleMetamodel: NotConvertedMetamodel): ConvertedMetamodel = {
    new ConvertedValueMetamodel(scalaType, _converter, _visibleMetamodel)
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

abstract class ConvertedMetamodel extends Metamodel { self =>
  protected def convert(_converter: Converter[Any, Any], _visibleMetamodel: NotConvertedMetamodel): ConvertedMetamodel = {
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
trait ContainerMetamodel extends Metamodel { self =>

  type M[A]

  def elementMetamodel: Metamodel
  implicit def functor: Functor[M]
  implicit def forEach: ForEach[M]

  require(scalaType.arguments.size == 1,
    "ScalaType of container must have only 1 argument (type parameter)")

  require(scalaType.arguments(0) == elementMetamodel.scalaType,
    "elementMetamodel doesn't match with the container type")

  def updateElementMetamodel(_elementMetamodel: Metamodel): Metamodel = {
    require(elementMetamodel.scalaType == _elementMetamodel.scalaType,
      "Cannot update element metamodel: expect metamodel for type %s, found %s".
        format(elementMetamodel.scalaType, _elementMetamodel.scalaType))

    _elementMetamodel match {
      case cv: ConvertedMetamodel =>
        new ContainerMetamodelWithConvertedElement[M](scalaType, cv)
      case mm @ _ =>
        new ContainerMetamodelImpl[M](scalaType, mm)
    }
  }

  def rewriteChildMetamodels(rules: Rules[Metamodel]) = updateElementMetamodel(elementMetamodel.rewrite(rules))

  private lazy val newBuilderF: () => Builder[Any, M[Any]] = {
    scalaType match {
      case at @ ArrayType(_) => Some({ () => at.newArrayBuilder[Any]().asInstanceOf[Builder[Any, M[Any]]] })
      case coll @ CollectionType(_) => coll.newBuilder.asInstanceOf[Option[() => Builder[Any, M[Any]]]]
      case _ => None
    }
  } getOrElse {
    sys.error("No builder is found for type " + scalaType)
  }

  def newBuilder() = newBuilderF()

  override def toString() = "ContainerMetamodel(%s, %s)".format(scalaType, elementMetamodel)
}

class ContainerMetamodelImpl[MM[_]](
  val scalaType: ScalaType,
  val elementMetamodel: Metamodel)(implicit val functor: Functor[MM], val forEach: ForEach[MM])
  extends NotConvertedMetamodel with ContainerMetamodel {

  type M[A] = MM[A]
}

object ContainerMetamodel {
  def unapply(cm: ContainerMetamodel) = Some(cm.elementMetamodel)
}

/**
 * Metamodel for homogeneous container types like Option, Array, Traversable
 * with element metamodel containing conversion.
 *
 *  Such models can be introspected in 2 ways: as converted container with visible type
 *  or as underlying container with converted values. Second option will in general be more
 *  efficient.
 */
class ContainerMetamodelWithConvertedElement[MM[_]](
  val scalaType: ScalaType,
  val elementMetamodel: ConvertedMetamodel)(implicit val functor: Functor[MM], val forEach: ForEach[MM])
  extends ConvertedMetamodel with ContainerMetamodel {

  type M[A] = MM[A]

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

  import org.scalastuff.util.Functors._
  import org.scalastuff.util.ForEach._

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
    val cm = metamodelOf(OptionType(elementMetamodel.scalaType)).
      asInstanceOf[ContainerMetamodel].updateElementMetamodel(elementMetamodel)
  }
}