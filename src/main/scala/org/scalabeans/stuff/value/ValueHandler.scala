package org.scalabeans.stuff.value

import com.dyuproject.protostuff.{Input, Pipe, Output}
import org.scalabeans._

abstract class ValueHandler {
  type V

  val inlined = false

  def defaultValue: V

  def isDefaultValue(value: V): Boolean = (value == defaultValue)

  def readFrom(input: Input): V

  def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean)

  def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean)
}

object ValueHandler {
  def apply(scalaType: ScalaType): Option[ValueHandler] = scalaType match {
    case IntType => Some(new ValueHandler {
      type V = Int

      val defaultValue: V = 0

      def readFrom(input: Input) = input.readInt32()

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeInt32(tag, value, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        output.writeInt32(tag, input.readInt32, repeated)
      }
    })

    case LongType => Some(new ValueHandler {
      type V = Long

      val defaultValue: V = 0L

      def readFrom(input: Input) = input.readInt64()

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeInt64(tag, value, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        output.writeInt64(tag, input.readInt64, repeated)
      }
    })

    case FloatType => Some(new ValueHandler {
      type V = Float

      val defaultValue: V = 0f

      def readFrom(input: Input) = input.readFloat()

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeFloat(tag, value, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        output.writeFloat(tag, input.readFloat, repeated)
      }
    })

    case DoubleType => Some(new ValueHandler {
      type V = Double

      val defaultValue: V = 0

      def readFrom(input: Input) = input.readDouble

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeDouble(tag, value, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        output.writeDouble(tag, input.readDouble, repeated)
      }
    })

    case BooleanType => Some(new ValueHandler {
      type V = Boolean

      val defaultValue: V = false

      def readFrom(input: Input) = input.readBool

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeBool(tag, value, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        output.writeBool(tag, input.readBool, repeated)
      }
    })

    case CharType => Some(new ValueHandler {
      type V = Char

      val defaultValue: V = 0

      def readFrom(input: Input): V = input.readUInt32().asInstanceOf[V]

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeUInt32(tag, value, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        output.writeUInt32(tag, input.readUInt32, repeated)
      }
    })

    case ShortType => Some(new ValueHandler {
      type V = Short

      val defaultValue: V = 0

      def readFrom(input: Input) = input.readUInt32().asInstanceOf[V]

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeUInt32(tag, value, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        output.writeUInt32(tag, input.readUInt32, repeated)
      }
    })

    case ByteType => Some(new ValueHandler {
      type V = Byte

      val defaultValue: V = 0

      def readFrom(input: Input) = input.readUInt32().asInstanceOf[V]

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeUInt32(tag, value, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        output.writeUInt32(tag, input.readUInt32, repeated)
      }
    })

    case StringType => Some(new ValueHandler {
      type V = String

      def defaultValue: V = ""

      def readFrom(input: Input) = input.readString()

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeString(tag, value, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        input.transferByteRangeTo(output, true, tag, repeated)
      }
    })

    case BigDecimalType => Some(new ValueHandler {
      type V = BigDecimal

      val defaultValue: V = 0

      def readFrom(input: Input) = BigDecimal(input.readString())

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeString(tag, value.toString, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        input.transferByteRangeTo(output, true, tag, repeated)
      }
    })

    case BigIntType => Some(new ValueHandler {
      type V = BigInt

      val defaultValue: V = 0

      def readFrom(input: Input) = BigInt(input.readString())

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeString(tag, value.toString, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        input.transferByteRangeTo(output, true, tag, repeated)
      }
    })

    case DateType => Some(new ValueHandler {
      type V = java.util.Date

      val defaultValue: V = new java.util.Date(0)

      override def isDefaultValue(v: V) = (v.getTime == 0)

      def readFrom(input: Input) = new java.util.Date(input.readFixed64())

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeFixed64(tag, value.getTime, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        output.writeFixed64(tag, input.readFixed64(), repeated)
      }
    })

    case SqlTimestampType => Some(new ValueHandler {
      type V = java.sql.Timestamp

      val defaultValue: V = new java.sql.Timestamp(0)

      override def isDefaultValue(v: V) = (v.getTime == 0)

      def readFrom(input: Input) = java.sql.Timestamp.valueOf(input.readString())

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeString(tag, value.toString, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        input.transferByteRangeTo(output, true, tag, repeated)
      }
    })

    case SqlDateType => Some(new ValueHandler {
      type V = java.sql.Date

      val defaultValue: V = new java.sql.Date(0)

      override def isDefaultValue(v: V) = (v.getTime == 0)

      def readFrom(input: Input) = java.sql.Date.valueOf(input.readString())

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = output.writeString(tag, value.toString, repeated)

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        input.transferByteRangeTo(output, true, tag, repeated)
      }
    })

    case EnumType(enum) => Some(new ValueHandler {
      type V = AnyRef

      val defaultValue: V = enum.values.head

      override def isDefaultValue(v: V) = false

      def readFrom(input: Input) = {
        val ordinal = input.readInt32()
        enum.valueOf(ordinal) map (_.constant) getOrElse error("Cannot read enum value of %s: unknown ordinal %d".format(enum.toString, ordinal))
      }

      def writeValueTo(tag: Int, output: Output, value: V, repeated: Boolean) = {
        output.writeInt32(tag, enum.valueOf(value) ordinal, repeated)
      }

      def transfer(tag: Int, pipe: Pipe, input: Input, output: Output, repeated: Boolean) {
        output.writeInt32(tag, input.readInt32, repeated)
      }
    })

    // TODO: Arrays

    case OptionType(valueType) =>
      for (valueHandler <- ValueHandler(valueType))
      yield new OptionValueHandler(WrappedValueHandler(valueHandler, valueType))

    case t@TraversableType(elementType) => t.newBuilder match {
      case Some(newBuilderFunction) =>
        for (elemValueHandler <- ValueHandler(elementType))
        yield
          new RepeatedValueHandler(WrappedValueHandler(elemValueHandler, elementType)) {
            def newBuilder() = newBuilderFunction().asInstanceOf[CB]
          }

      case _ => None
    }

    case r: AnyRefType => Some(BeanValueHandler(r))


  }
}