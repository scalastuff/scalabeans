package org.scalastuff.scalabeans.format.proto

import org.scalastuff.proto.value.BeanValueHandler
import org.scalastuff.proto.value.ValueHandler
import org.scalastuff.scalabeans.Preamble._
import org.scalastuff.scalabeans.types.ScalaType
import com.dyuproject.protostuff.Schema
import org.scalastuff.scalabeans.FormatFactory
import org.scalastuff.util.Format
import org.scalastuff.util.Rules
import org.scalastuff.scalabeans.Metamodel

object ProtobufFormatFactory {
  def apply(rules: Rules[Metamodel] = EmptyMetamodelRules) = new ProtobufFormatFactory(rules)
}

class ProtobufFormatFactory private(rules: Rules[Metamodel]) extends FormatFactory {
  type This = ProtobufFormatFactory
  type F[A] = Format[A]

  def withRewriteRules(rules: Rules[Metamodel]) = new ProtobufFormatFactory(rules)
    
  protected def formatFor[T <: AnyRef : Manifest](metamodel: Metamodel): Format[T] = {
    val updatedMetamodel = metamodel.rewrite(rules)
    ValueHandler(updatedMetamodel) match {
      case Some(beanValueHandler: BeanValueHandler) => new ProtobufFormat[T](beanValueHandler)
      case Some(_) => new WrappedProtobufFormat[T](updatedMetamodel) 
      case None => throw new RuntimeException("Cannot create protobuf format for type %s: this type or one of the type arguments is not supported".format(updatedMetamodel))
    }
  }
}