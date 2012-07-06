package org.scalastuff.scalabeans.format.json
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.core.JsonToken
import com.fasterxml.jackson.core.SerializableString
import org.scalastuff.scalabeans._
import org.scalastuff.scalabeans.types._
import com.fasterxml.jackson.core.io.SerializedString
import java.lang.{ Enum => JEnum }
import scala.collection.mutable.Builder
import org.scalastuff.scalabeans.converters.Converter

trait JsonHandler {
  def parse(parser: JsonParser): Any
  def write(generator: JsonGenerator, obj: Any)
}

object JsonHandler {
  def apply(metamodel: MetaModel): JsonHandler = {
    metamodel match {
      case cm @ ContainerMetaModel(elementMetaModel) => // also covers ContainerMetaModelWithConvertedElement 
        cm.scalaType match {
          case OptionType(_) => new OptionValueHandler(apply(elementMetaModel))
          case _ => new ArrayValueHandler(apply(elementMetaModel), cm)
        }
      
      case cv: ConvertedMetaModel => 
        new ConvertedValueHandler(apply(cv.visibleMetaModel), cv.converter)
        
      case ValueMetaModel(scalaType) =>
        scalaType match {
          case ByteType => ByteValueHandler
          case ShortType => ShortValueHandler
          case IntType => IntValueHandler
          case LongType => LongValueHandler
          case FloatType => FloatValueHandler
          case DoubleType => DoubleValueHandler
          case BigDecimalType => BigDecimalValueHandler
          case BigIntType => BigIntValueHandler
          case CharType => CharValueHandler
          case StringType => StringValueHandler
          case BooleanType => BooleanValueHandler
        }

      case bd: BeanDescriptor => new BeanHandler(bd)
    }
  }
}

class ConvertedValueHandler(visibleValueHandler: JsonHandler, converter: Converter[Any, Any]) extends JsonHandler {
  def parse(parser: JsonParser): Any = converter.from(visibleValueHandler.parse(parser))
  
  def write(generator: JsonGenerator, obj: Any) = visibleValueHandler.write(generator, converter.to(obj))
}

object ByteValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_NUMBER_INT => parser.getByteValue()
      case _ => new UnexpectedTokenException("integer number", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeNumber(obj.asInstanceOf[Byte])
  }
}

object ShortValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_NUMBER_INT => parser.getShortValue()
      case _ => new UnexpectedTokenException("integer number", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeNumber(obj.asInstanceOf[Short])
  }
}

object IntValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_NUMBER_INT => parser.getIntValue()
      case _ => new UnexpectedTokenException("integer number", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeNumber(obj.asInstanceOf[Int])
  }
}

object LongValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_NUMBER_INT => parser.getLongValue()
      case _ => new UnexpectedTokenException("integer number", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeNumber(obj.asInstanceOf[Long])
  }
}

object FloatValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_NUMBER_FLOAT => parser.getFloatValue()
      case JsonToken.VALUE_NUMBER_INT => parser.getFloatValue()
      case _ => new UnexpectedTokenException("number", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeNumber(obj.asInstanceOf[Float])
  }
}

object DoubleValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_NUMBER_FLOAT => parser.getDoubleValue()
      case JsonToken.VALUE_NUMBER_INT => parser.getDoubleValue()
      case _ => new UnexpectedTokenException("number", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeNumber(obj.asInstanceOf[Double])
  }
}

object BigDecimalValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_NUMBER_FLOAT => BigDecimal(parser.getDecimalValue())
      case JsonToken.VALUE_NUMBER_INT => BigDecimal(parser.getDecimalValue())
      case _ => new UnexpectedTokenException("number", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeNumber(obj.asInstanceOf[BigDecimal].underlying)
  }
}

object BigIntValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_NUMBER_INT => new math.BigInt(parser.getBigIntegerValue())
      case _ => new UnexpectedTokenException("integer number", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeNumber(obj.asInstanceOf[math.BigInt].underlying)
  }
}

object CharValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_NUMBER_INT =>
        parser.getIntValue().asInstanceOf[Char]

      case JsonToken.VALUE_STRING =>
        val c = parser.getText()
        if (c.length() != 1)
          throw new UnexpectedInputException("String with 1 character", "\"%s\"".format(c), parser.getCurrentLocation())
        c(0)

      case _ => new UnexpectedTokenException("char or integer number", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeString(obj.asInstanceOf[Char].toString())
  }
}

object StringValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_STRING => parser.getText()
      case _ => new UnexpectedTokenException("string value", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeString(obj.asInstanceOf[String])
  }
}

object BooleanValueHandler extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_TRUE => true
      case JsonToken.VALUE_FALSE => false
      case _ => new UnexpectedTokenException("boolean value", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeBoolean(obj.asInstanceOf[Boolean])
  }
}

class OptionValueHandler(elementValueHandler: JsonHandler) extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    if (parser.getCurrentToken() == JsonToken.VALUE_NULL) None
    else Some(elementValueHandler.parse(parser))
  }

  def write(generator: JsonGenerator, obj: Any) {
    if (None == obj) generator.writeNull()
    else elementValueHandler.write(generator, obj.asInstanceOf[Some[_]].get)
    
  }
}

class ArrayValueHandler(elementValueHandler: JsonHandler, metamodel: ContainerMetaModel) extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    if (parser.getCurrentToken != JsonToken.START_ARRAY)
      throw new UnexpectedTokenException(JsonToken.START_ARRAY.asString(), parser)

    val builder = metamodel.newBuilder()
    while (parser.nextToken() != JsonToken.END_ARRAY) {
      builder += elementValueHandler.parse(parser)
    }

    builder.result()
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeStartArray()

    metamodel.forEach(obj.asInstanceOf[metamodel.M[Any]]) { element =>
      elementValueHandler.write(generator, element)
    }

    generator.writeEndArray()
  }
}

case class Field(propertyDescriptor: PropertyDescriptor, handler: JsonHandler, name: SerializableString)

class BeanHandler(bd: BeanDescriptor) extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    if (parser.getCurrentToken != JsonToken.START_OBJECT)
      throw new UnexpectedTokenException(JsonToken.START_OBJECT.asString(), parser)

    val builder = bd.newBuilder()
    while (parser.nextToken() != JsonToken.END_OBJECT) {
      if (parser.getCurrentToken() != JsonToken.FIELD_NAME)
        throw new UnexpectedTokenException("field name", parser)

      val fieldName = parser.getCurrentName()
      fields.find(_.name.getValue() == fieldName) match {
        case Some(field) =>
          parser.nextToken()
          builder.set(field.propertyDescriptor, field.handler.parse(parser))
        case None =>
          throw new UnreadableInputException("unknown attribute %s found".format(parser.getCurrentName()), parser.getCurrentLocation())
      }
    }

    builder.result()
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeStartObject()
    for (field <- fields) {
      generator.writeFieldName(field.name)
      field.handler.write(generator, field.propertyDescriptor.get[Any](obj.asInstanceOf[AnyRef]))
    }
    generator.writeEndObject()
  }

  private[this] lazy val fields =
    for (pd <- bd.properties if pd.isInstanceOf[DeserializablePropertyDescriptor])
      yield Field(pd, JsonHandler(pd.typeMetaModel), new SerializedString(pd.name))
}
