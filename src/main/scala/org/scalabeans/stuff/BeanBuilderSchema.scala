package org.scalabeans.stuff

import value.{ValueHandler, RepeatedValueHandler, MutableBeanValueHandler}
import org.scalabeans._

class BeanBuilderSchema(_beanDescriptor: BeanDescriptor, override val fields: Seq[PropertyBuilderField])
  extends MirrorSchema[BeanBuilder](_beanDescriptor, fields) {

  override def newMessage = beanDescriptor.newBuilder

  val writeSchema = {
    val writeFields = fields map (_.readOnlyField)

    new WriteMirrorSchema[AnyRef](beanDescriptor, writeFields)
  }
}

object BeanBuilderSchema {
  def apply(beanDescriptor: BeanDescriptor) = {
    val properties = beanDescriptor.properties collect {
      case cp: ConstructorParameter => cp
      case mp: MutablePropertyDescriptor => mp
    }

    val fields =
      for {
        property <- properties
        field <- PropertyBuilderField(property.tag, property)
      } yield field

    new BeanBuilderSchema(beanDescriptor, fields)
  }
}

abstract class PropertyBuilderField(tag: Int, propertyDescriptor: PropertyDescriptor)
  extends MutableField[BeanBuilder](tag, propertyDescriptor) {

  def readOnlyField = MirrorField[AnyRef](tag, propertyDescriptor).get // do we ever get None here?

  def getValue(message: BeanBuilder) = message.get(propertyDescriptor).asInstanceOf[valueHandler.V]

  def setValue(message: BeanBuilder, value: valueHandler.V) {
    message.set(propertyDescriptor, value)
  }
}

object PropertyBuilderField {
  def apply(tag: Int, prop: PropertyDescriptor) =
      for (valueHandler <- ValueHandler(prop.scalaType))
      yield {
        valueHandler match {
          case repeatedValueHandler: RepeatedValueHandler =>
            new PropertyBuilderField(tag, prop) with RepeatedField[BeanBuilder] {
              val valueHandler = repeatedValueHandler
            }
          case beanValueHandler: MutableBeanValueHandler =>
            new PropertyBuilderField(tag, prop) with BeanField[BeanBuilder] {
              val valueHandler = beanValueHandler
            }
          case vh @ _ =>
            new PropertyBuilderField(tag, prop) {
              val valueHandler = vh
            }
        }
      }
}
