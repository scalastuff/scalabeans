package org.scalastuff.scalabeans.format.proto

import org.scalastuff.proto.value.BeanValueHandler
import org.scalastuff.proto.value.ValueHandler
import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.scalabeans.types.ScalaType
import org.scalastuff.scalabeans.Rules
import com.dyuproject.protostuff.Schema
import org.scalastuff.scalabeans.FormatFactory
import org.scalastuff.util.Format

object ProtobufFormatFactory {
  def apply(rewriteRules: Rules[ScalaType] = EmptyScalaTypeRules) = new ProtobufFormatFactory(rewriteRules)
}

class ProtobufFormatFactory private(rewriteRules: Rules[ScalaType] = EmptyScalaTypeRules) extends FormatFactory {
  type This = ProtobufFormatFactory
  type F[A] = Format[A]

  def withRewriteRules(scalaTypeRules: Rules[ScalaType]) = new ProtobufFormatFactory(scalaTypeRules)
    
  protected def formatFor[T <: AnyRef : Manifest](_scalaType: ScalaType): Format[T] = {
    val scalaType = _scalaType.rewrite(rewriteRules)
    ValueHandler(scalaType) match {
      case Some(beanValueHandler: BeanValueHandler) => new ProtobufFormat[T](beanValueHandler)
      case Some(_) => new WrappedProtobufFormat[T](scalaType) 
      case None => throw new RuntimeException("Cannot create protobuf format for type %s: this type or one of the type arguments is not supported".format(scalaType))
    }
  }
}