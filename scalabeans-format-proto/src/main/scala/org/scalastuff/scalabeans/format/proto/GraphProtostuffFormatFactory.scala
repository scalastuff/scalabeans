package org.scalastuff.scalabeans.format.proto

import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.scalabeans._
import org.scalastuff.scalabeans.types.ScalaType
import org.scalastuff.util.Format
import org.scalastuff.util.Rules
import org.scalastuff.proto.value.BeanValueHandler
import org.scalastuff.proto.value.ValueHandler
import org.scalastuff.util.Converter

object GraphProtostuffFormatFactory {
  val defaultRewriteRules = metamodelRules {
    case mm @ ValueMetamodel(scalaType) if scalaType.erasure == classOf[java.util.UUID] =>
      mm.convert(Converter(
        { uuid: java.util.UUID =>
          if (uuid == null) None
          else Some((uuid.getMostSignificantBits(), uuid.getLeastSignificantBits()))
        },
        { bitsO: Option[(Long, Long)] =>
          bitsO match {
            case Some(bits) => new java.util.UUID(bits._1, bits._2)
            case None => null
          }          
        }))
  }

  def apply(rules: Rules[Metamodel] = defaultRewriteRules) = new GraphProtostuffFormatFactory(rules)
}

class GraphProtostuffFormatFactory private (rules: Rules[Metamodel]) extends FormatFactory {
  type This = GraphProtostuffFormatFactory
  type F[A] = Format[A]

  def withRewriteRules(rules: Rules[Metamodel]) = new GraphProtostuffFormatFactory(rules)

  protected def formatFor[T <: AnyRef: Manifest](metamodel: Metamodel): Format[T] = {
    val scalaType = metamodel.rewrite(rules)
    ValueHandler(scalaType) match {
      case Some(beanValueHandler: BeanValueHandler) => new GraphProtostuffFormat[T](beanValueHandler)
      case Some(_) => new WrappedGraphProtostuffFormat[T](scalaType)
      case None => throw new RuntimeException("Cannot create graph protostuff format for type %s: this type or one of the type arguments is not supported".format(scalaType))
    }
  }
}