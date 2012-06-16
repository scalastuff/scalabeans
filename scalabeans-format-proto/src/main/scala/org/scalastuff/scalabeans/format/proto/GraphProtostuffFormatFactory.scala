package org.scalastuff.scalabeans.format.proto

import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.scalabeans._
import org.scalastuff.scalabeans.types.ScalaType
import org.scalastuff.util.Format
import org.scalastuff.proto.value.BeanValueHandler
import org.scalastuff.proto.value.ValueHandler

object GraphProtostuffFormatFactory {
  val defaultRewriteRules = propertyRules {
    case p @ PropertyDescriptor(_, t) if t.erasure == classOf[java.util.UUID] =>
      p.convertValue(
        { uuid: java.util.UUID =>
          if (uuid == null) None
          else Some((uuid.getMostSignificantBits(), uuid.getLeastSignificantBits()))
        },
        { bitsO: Option[(Long, Long)] =>
          bitsO match {
            case Some(bits) => new java.util.UUID(bits._1, bits._2)
            case None => null
          }          
        })
  }

  def apply(rewriteRules: Rules[ScalaType] = defaultRewriteRules) = new GraphProtostuffFormatFactory(rewriteRules)
}

class GraphProtostuffFormatFactory private (rewriteRules: Rules[ScalaType]) extends FormatFactory {
  type This = GraphProtostuffFormatFactory
  type F[A] = Format[A]

  def withRewriteRules(scalaTypeRules: Rules[ScalaType]) = new GraphProtostuffFormatFactory(scalaTypeRules)

  protected def formatFor[T <: AnyRef: Manifest](_scalaType: ScalaType): Format[T] = {
    val scalaType = _scalaType.rewrite(rewriteRules)
    ValueHandler(scalaType) match {
      case Some(beanValueHandler: BeanValueHandler) => new GraphProtostuffFormat[T](beanValueHandler)
      case Some(_) => new WrappedGraphProtostuffFormat[T](scalaType)
      case None => throw new RuntimeException("Cannot create graph protostuff format for type %s: this type or one of the type arguments is not supported".format(scalaType))
    }
  }
}