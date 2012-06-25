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

trait JsonHandler {
  def parse(parser: JsonParser): Any
  def write(generator: JsonGenerator, obj: Any)
}

trait NullableValueHandler extends JsonHandler {
  @inline def nullValue: Any = null

  abstract override def parse(parser: JsonParser): Any = {
    if (parser.getCurrentToken() == JsonToken.VALUE_NULL) nullValue
    else super.parse(parser)
  }

  abstract override def write(generator: JsonGenerator, obj: Any) {
    if (nullValue == obj) generator.writeNull()
    else super.write(generator, obj)
  }
}

object JsonHandler {
  def apply(metamodel: Metamodel): JsonHandler = {
    metamodel.visibleMetamodel match {
      case ValueMetamodel(scalaType) =>
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
          case SqlTimestampType => SqlTimestampValueHandler
          case SqlDateType => SqlDateValueHandler
          case DateType => DateValueHandler
          case EnumType(enum) => EnumValueHandler(enum)
          case JavaEnumType(values) => JavaEnumValueHandler(values)
        }

      case cm @ ContainerMetamodel(elementMetamodel) =>
        cm.scalaType match {
          case OptionType(_) => OptionValueHandler(apply(elementMetamodel))
          case _ => ArrayValueHandler(apply(elementMetamodel), cm)
        }

      case bd: BeanDescriptor => BeanHandler(bd)
    }
  }
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

class BigDecimalValueHandler private () extends JsonHandler {
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
object BigDecimalValueHandler extends BigDecimalValueHandler with NullableValueHandler

class BigIntValueHandler private () extends JsonHandler {
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
object BigIntValueHandler extends BigIntValueHandler with NullableValueHandler

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

class StringValueHandler private () extends JsonHandler {
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
object StringValueHandler extends StringValueHandler with NullableValueHandler

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

class SqlDateValueHandler private () extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_STRING => java.sql.Date.valueOf(parser.getText())
      case _ => new UnexpectedTokenException("string date value", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeString(obj.toString)
  }
}
object SqlDateValueHandler extends SqlDateValueHandler with NullableValueHandler

class SqlTimestampValueHandler private () extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_STRING => java.sql.Timestamp.valueOf(parser.getText())
      case _ => new UnexpectedTokenException("string timestamp value", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeString(obj.toString)
  }
}
object SqlTimestampValueHandler extends SqlTimestampValueHandler with NullableValueHandler

class DateValueHandler private () extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_NUMBER_INT => new java.util.Date(parser.getLongValue())
      case _ => new UnexpectedTokenException("integer milliseconds value", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeNumber(obj.asInstanceOf[java.util.Date].getTime())
  }
}
object DateValueHandler extends DateValueHandler with NullableValueHandler

class EnumValueHandler private (enum: Enum[AnyRef]) extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_STRING =>
        val name = parser.getText()
        enum.valueOf(name) getOrElse {
          throw new UnexpectedInputException("One of " + enum.names.mkString("[", ",", "]"), name, parser.getCurrentLocation())
        }
      case JsonToken.VALUE_NULL => null
      case _ => new UnexpectedTokenException("string value", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeString(enum.nameOf(obj.asInstanceOf[AnyRef]))
  }
}
object EnumValueHandler {
  def apply(enum: Enum[AnyRef]) = new EnumValueHandler(enum) with NullableValueHandler
}

class JavaEnumValueHandler(values: Iterable[JEnum[_]]) extends JsonHandler {
  def parse(parser: JsonParser): Any = {
    parser.getCurrentToken match {
      case JsonToken.VALUE_STRING =>
        val name = parser.getText()
        values.find(_.name == name) getOrElse {
          throw new UnexpectedInputException("one of " + values.map(_.name).mkString("[", ",", "]"), name, parser.getCurrentLocation())
        }
      case JsonToken.VALUE_NULL => null
      case _ => new UnexpectedTokenException("string value", parser)
    }
  }

  def write(generator: JsonGenerator, obj: Any) {
    generator.writeString(obj.asInstanceOf[JEnum[_]].name)
  }
}
object JavaEnumValueHandler {
  def apply(values: Iterable[JEnum[_]]) = new JavaEnumValueHandler(values) with NullableValueHandler
}

class OptionValueHandler private (elementValueHandler: JsonHandler) extends JsonHandler {
  def parse(parser: JsonParser): Any = Some(elementValueHandler.parse(parser))

  def write(generator: JsonGenerator, obj: Any) {
    elementValueHandler.write(generator, obj.asInstanceOf[Some[_]].get)
  }
}

object OptionValueHandler {
  def apply(elementValueHandler: JsonHandler) = new OptionValueHandler(elementValueHandler) with NullableValueHandler {
    @inline override def nullValue = None
  }
}

class ArrayValueHandler private (elementValueHandler: JsonHandler, metamodel: ContainerMetamodel) extends JsonHandler {
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
object ArrayValueHandler {
  def apply(elementValueHandler: JsonHandler, metamodel: ContainerMetamodel) =
    new ArrayValueHandler(elementValueHandler, metamodel) with NullableValueHandler
}

case class Field(propertyDescriptor: PropertyDescriptor, handler: JsonHandler, name: SerializableString)

class BeanHandler private (bd: BeanDescriptor) extends JsonHandler {
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
      yield Field(pd, JsonHandler(pd.metamodel), new SerializedString(pd.name))
}
object BeanHandler {
  def apply(bd: BeanDescriptor) = new BeanHandler(bd) with NullableValueHandler
}

