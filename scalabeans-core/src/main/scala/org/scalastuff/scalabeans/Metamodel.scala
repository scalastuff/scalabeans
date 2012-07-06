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
 *  perform conversions only when you deal with ConvertedValueMetaModel.
 */
abstract class MetaModel { self =>
  val scalaType: ScalaType

  val visibleMetaModel: NotConvertedMetaModel

  val converter: Converter[Any, Any]

  @inline final def addConverter[A: Manifest, B: Manifest](converter: Converter[A, B]): ConvertedMetaModel =
    addConverter(RuntimeConverter(scalaTypeOf[A], scalaTypeOf[B], converter))

  final def addConverter(runtimeConverter: RuntimeConverter): ConvertedMetaModel = {
    require(visibleMetaModel.scalaType == runtimeConverter.sourceType,
      "Cannot create ConvertedMetaModel: source type %s of the converter doesn't match with the visible ScalaType %s of this meta model".
        format(runtimeConverter.sourceType, visibleMetaModel.scalaType))

    addConverter(runtimeConverter.converter.asInstanceOf[Converter[Any, Any]], metaModelOf(runtimeConverter.targetType))
  }

  protected def addConverter(converter: Converter[Any, Any], _visibleMetaModel: NotConvertedMetaModel): ConvertedMetaModel

  def toVisible(underlying: Any) = converter.to(underlying)
  def toUnderlying(visible: Any) = converter.from(visible)

  final def rewrite(rules: Rules[MetaModel]): MetaModel = {
    val result = rules(this)

    require(this.scalaType == result.scalaType,
      "Cannot rewrite: expect meta model for type %s, found %s".format(this.scalaType, result.scalaType))

    result.rewriteChildMetaModels(rules)
  }

  def rewriteChildMetaModels(rules: Rules[MetaModel]): MetaModel

  override def toString() = "MetaModel(%s)".format(scalaType)
}

abstract class NotConvertedMetaModel extends MetaModel {
  final val visibleMetaModel = this
  final val converter = Converter.identity[Any]

  protected def addConverter(_converter: Converter[Any, Any], _visibleMetaModel: NotConvertedMetaModel): ConvertedMetaModel = {
    new ConvertedValueMetaModel(scalaType, _converter, _visibleMetaModel)
  }
}

/**
 * Represents meta model of a value.
 *
 * Never contains conversions.
 */
final case class ValueMetaModel(val scalaType: ScalaType) extends NotConvertedMetaModel {
  def rewriteChildMetaModels(rules: Rules[MetaModel]) = this
}

abstract class ConvertedMetaModel extends MetaModel { self =>
  protected def addConverter(_converter: Converter[Any, Any], _visibleMetaModel: NotConvertedMetaModel): ConvertedMetaModel = {
    new ConvertedValueMetaModel(
      scalaType,
      converter.compose(_converter.asInstanceOf[Converter[Any, Any]]),
      _visibleMetaModel)
  }
}

class ConvertedValueMetaModel private[scalabeans] (
  val scalaType: ScalaType,
  val converter: Converter[Any, Any],
  val visibleMetaModel: NotConvertedMetaModel) extends ConvertedMetaModel {

  def rewriteChildMetaModels(rules: Rules[MetaModel]) = (visibleMetaModel rewrite rules) match {
    case cv: ConvertedMetaModel =>
      addConverter(cv.converter, cv.visibleMetaModel)
    case mm: NotConvertedMetaModel => new ConvertedValueMetaModel(scalaType, converter, mm)
  }

  override def toString() = "ConvertedValueMetaModel(%s, %s)".format(scalaType, visibleMetaModel)
}

/**
 * Meta model for homogeneous container types like Option, Array, Traversable.
 *
 * N.B. Meta model of Tuple1 is a BeanDescriptor
 */
trait ContainerMetaModel extends MetaModel { self =>

  type M[A]

  def elementMetaModel: MetaModel
  implicit def functor: Functor[M]
  implicit def forEach: ForEach[M]

  require(scalaType.arguments.size == 1,
    "ScalaType of container must have only 1 argument (type parameter)")

  require(scalaType.arguments(0) == elementMetaModel.scalaType,
    "elementMetaModel doesn't match with the container type")

  def updateElementMetaModel(_elementMetaModel: MetaModel): MetaModel = {
    require(elementMetaModel.scalaType == _elementMetaModel.scalaType,
      "Cannot update elementMetaModel: expect meta model for type %s, found %s".
        format(elementMetaModel.scalaType, _elementMetaModel.scalaType))

    _elementMetaModel match {
      case cv: ConvertedMetaModel =>
        new ContainerMetaModelWithConvertedElement[M](scalaType, cv)
      case mm @ _ =>
        new ContainerMetaModelImpl[M](scalaType, mm)
    }
  }

  def rewriteChildMetaModels(rules: Rules[MetaModel]) = updateElementMetaModel(elementMetaModel.rewrite(rules))

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

  override def toString() = "ContainerMetaModel(%s, %s)".format(scalaType, elementMetaModel)
}

class ContainerMetaModelImpl[MM[_]](
  val scalaType: ScalaType,
  val elementMetaModel: MetaModel)(implicit val functor: Functor[MM], val forEach: ForEach[MM])
  extends NotConvertedMetaModel with ContainerMetaModel {

  type M[A] = MM[A]
}

object ContainerMetaModel {
  def unapply(cm: ContainerMetaModel) = Some(cm.elementMetaModel)
}

/**
 * Meta model for homogeneous container types like Option, Array, Traversable
 * with element meta model containing conversion.
 *
 *  Such models can be introspected in 2 ways: as converted container with visible type
 *  or as underlying container with converted values. Second option will in general be more
 *  efficient.
 */
class ContainerMetaModelWithConvertedElement[MM[_]](
  val scalaType: ScalaType,
  val elementMetaModel: ConvertedMetaModel)(implicit val functor: Functor[MM], val forEach: ForEach[MM])
  extends ConvertedMetaModel with ContainerMetaModel {

  type M[A] = MM[A]

  /**
   * Provides conversion of the container to/from visible type using fmap and element converter.
   */
  val converter = new Converter[M[_], M[_]] {
    def to(underlying: M[_]) = functor.fmap(underlying, (elementMetaModel.toVisible _))
    def from(visible: M[_]) = functor.fmap(visible, (elementMetaModel.toUnderlying _))
  }.asInstanceOf[Converter[Any, Any]]

  val visibleMetaModel = metaModelOf(ScalaType(scalaType.erasure, Seq(elementMetaModel.visibleMetaModel.scalaType)))
}

object MetaModel {

  import org.scalastuff.util.Functors._
  import org.scalastuff.util.ForEach._

  def apply[A: Manifest](): NotConvertedMetaModel = apply(scalaTypeOf[A])
  def apply(scalaType: ScalaType): NotConvertedMetaModel = (scalaType match {
    case OptionType(elementType) => new ContainerMetaModelImpl[Option](scalaType, apply(elementType))
    case ArrayType(elementType) => new ContainerMetaModelImpl[Array](scalaType, apply(elementType))
    case TraversableType(elementType) => new ContainerMetaModelImpl[Traversable](scalaType, apply(elementType))
    case BeanType() => BeanDescriptor(scalaType)
    case _ => ValueMetaModel(scalaType)
  })

  def unapply(metaModel: MetaModel) = Some(metaModel.visibleMetaModel.scalaType)
}

trait MetaModels {
  def TupleBeanDescriptor(propertyMetaModel: MetaModel) = {
    val bd = descriptorOf(TupleType(propertyMetaModel.scalaType))
    bd mapProperties {
      case p @ PropertyDescriptor("_1", _) => p.withTypeMetaModel(propertyMetaModel)
    }
  }

  def TupleBeanDescriptor(keyMetaModel: MetaModel, valueMetaModel: MetaModel) = {
    val bd = descriptorOf(TupleType(keyMetaModel.scalaType, valueMetaModel.scalaType))
    bd mapProperties {
      case p @ PropertyDescriptor("_1", _) => p.withTypeMetaModel(keyMetaModel)
      case p @ PropertyDescriptor("_2", _) => p.withTypeMetaModel(valueMetaModel)
    }
  }

  def OptionMetaModel(elementMetaModel: MetaModel) = {
    val cm = metaModelOf(OptionType(elementMetaModel.scalaType)).
      asInstanceOf[ContainerMetaModel].updateElementMetaModel(elementMetaModel)
  }
}