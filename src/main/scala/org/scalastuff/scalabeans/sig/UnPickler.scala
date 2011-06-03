/**
 * Copyright (c) 2011 ScalaStuff.org (joint venture of Alexander Dvorkovyy and Ruud Diterwich)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package org.scalastuff.scalabeans.sig
import scala.reflect.ScalaSignature
import scala.reflect.ScalaLongSignature
import java.nio.charset.Charset
import scala.reflect.generic.ByteCodecs
import scala.reflect.generic.PickleFormat
import annotation.switch
import java.util.Arrays
import scala.collection.mutable.ArrayBuffer

/**
 * Loads Scala Signature from annotation and parses to Array of basic data structures from PickleFormat.
 */
object UnPickler extends Application {
  val debug = false

  /**
   * Common trait for all data structures in PickleFormat
   */
  trait Entry

  trait NameEntry extends Entry {
    def name: String
  }

  trait EntryRef extends Entry {
    def table: Array[Entry]
  }

  trait SymbolEntry extends EntryRef {
    def nameRef: Int
    def ownerRef: Option[Int]
    lazy val name = table(nameRef).asInstanceOf[NameEntry].name
    lazy val ownerOption = ownerRef map table flatMap (_ match {
      case se: SymbolEntry => Some(se)
      case _ => None
    })
    lazy val path: List[String] = name :: (ownerOption map (_.path) getOrElse Nil)
  }
  trait ExternalSymbolEntry extends SymbolEntry

  trait SymbolDeclEntry extends SymbolEntry {
    def symbolInfo: SymbolInfo
    val nameRef = symbolInfo.nameRef
    val ownerRef = Some(symbolInfo.ownerRef)
    lazy val owner = table(symbolInfo.ownerRef).asInstanceOf[SymbolEntry]
    val flags = symbolInfo.flags
    lazy val typeEntry: TypeEntry = table(symbolInfo.infoRef) match {
      case te: TypeEntry => te
      case x @ _ =>
        val typeRef = symbolInfo.privateWithinRef getOrElse (throw new RuntimeException("unexpected entry " + x))        
        table(typeRef).asInstanceOf[TypeEntry]
    }
  }

  object SymbolDeclEntry {
    def unapply(se: SymbolDeclEntry) = Some(se.symbolInfo)
  }

  trait TypeEntry extends Entry {
    def isValueType: Boolean
  }

  trait TypeEntryRef extends TypeEntry with EntryRef
  trait TypeSymEntryRef extends TypeEntryRef {
    def symRef: Int
    lazy val symbolEntry = table(symRef).asInstanceOf[SymbolEntry]
  }
  trait TypeSymsEntryRef extends TypeEntryRef {
    def symRefs: Seq[Int]
    lazy val symbolEntries =
      for (symRef <- symRefs) yield table(symRef).asInstanceOf[SymbolEntry]

  }
  trait TypeWithParentsEntryRef extends TypeEntryRef {
    def typeRefs: Seq[Int]
    lazy val parentEntries =
      for (parentRef <- typeRefs) yield table(parentRef).asInstanceOf[TypeEntry]

  }
  trait CompoundTypeEntryRef extends TypeEntryRef {
    def typeRef: Int
    lazy val typeEntry = table(typeRef).asInstanceOf[TypeEntry]
  }

  type SymbolInfo = PickleBuffer#SymbolInfo

  case class TermName(name: String) extends NameEntry
  case class TypeName(name: String) extends NameEntry
  case class NoneSym extends Entry
  abstract case class TypeSym(symbolInfo: SymbolInfo) extends SymbolDeclEntry
  abstract case class AliasSym(symbolInfo: SymbolInfo) extends SymbolDeclEntry
  abstract case class ClassSym(symbolInfo: SymbolInfo, thisTypeRef: Option[Int]) extends SymbolDeclEntry
  abstract case class ModuleSym(symbolInfo: SymbolInfo) extends SymbolDeclEntry
  abstract case class ValSym(symbolInfo: SymbolInfo, aliasRef: Option[Int]) extends SymbolDeclEntry
  abstract case class ExtRef(nameRef: Int, ownerRef: Option[Int]) extends ExternalSymbolEntry
  abstract case class ExtModClassRef(nameRef: Int, ownerRef: Option[Int]) extends ExternalSymbolEntry

  case class NoType extends TypeEntry { final val isValueType = false }
  case class NoPrefixType extends TypeEntry { final val isValueType = false }
  abstract case class ThisType(symRef: Int) extends TypeSymEntryRef { final val isValueType = false } // Path in TSLS
  abstract case class SingleType(typeRef: Int, symRef: Int) extends TypeSymEntryRef with CompoundTypeEntryRef { final val isValueType = false }// Singleton in TSLS (path, sym)
  case class ConstantType(constantRef: Int) extends TypeEntry { final val isValueType = true }

  // Type projection / designator in TSLS (path, sym, args)
  abstract case class TypeRefType(typeRef: Int, symRef: Int, targRef: Seq[Int]) extends TypeSymEntryRef with CompoundTypeEntryRef {
    def isValueType = symbolEntry match {
      case sde: SymbolDeclEntry => sde.typeEntry.isValueType
      case er: ExtRef => true
      case _ => false
    }
    lazy val typeParameterEntries =
      for (typeRef <- targRef) yield table(typeRef).asInstanceOf[TypeEntry]
  }
  abstract case class TypeBoundsType(lowerTypeRef: Int, upperTypeRef: Int) extends TypeEntryRef {
    final val isValueType = true
    lazy val lowerTypeEntry = table(lowerTypeRef).asInstanceOf[TypeEntry]
    lazy val upperTypeEntry = table(upperTypeRef).asInstanceOf[TypeEntry]
  }
  
  // (sym (cyclic ref), parents)
  abstract case class RefinedType(symRef: Int, typeRefs: Seq[Int]) extends TypeSymEntryRef with TypeWithParentsEntryRef {
    final val isValueType = true
  }

  // (sym (cyclic ref), parents)
  abstract case class ClassInfoType(symRef: Int, typeRefs: Seq[Int]) extends TypeSymEntryRef with TypeWithParentsEntryRef {
    final val isValueType = true
  }
  
  abstract case class MethodType(typeRef: Int, symRefs: Seq[Int]) extends CompoundTypeEntryRef with TypeSymsEntryRef {
    final val isValueType = false
  }
  
  // (classInfo, args)
  abstract case class PolyType(typeRef: Int, symRefs: Seq[Int]) extends CompoundTypeEntryRef with TypeSymsEntryRef {
    def isValueType = table(typeRef) match {
      case te:TypeEntry => te.isValueType
      case _ => false
    }
  }
  abstract case class SuperType(thisTypeRef: Int, superTypeRef: Int) extends TypeEntryRef {
    final val isValueType = true
  }
  abstract case class ExistentialType(typeRef: Int, symRefs: Seq[Int]) extends CompoundTypeEntryRef with TypeSymsEntryRef {
    final val isValueType = true
  }

  case class Children(symRef: Int, symRefs: Seq[Int]) extends Entry
  case class TODOEntry(tag: Int) extends Entry
  case class BrokenEntry(index: Int, entry: Entry, bytes: Array[Byte]) extends Entry {
    override def toString = "broken: %s at %d, bytes= %s".format(entry.toString, index,
      bytes map { b => "%#04x".format(b) } mkString ("[", ",", "]"))
  }

  def read(clazz: String): Option[Array[Entry]] = read(Class.forName(clazz))
  def read(clazz: Class[_]): Option[Array[Entry]] = {
    for {
      bytes <- getSigBytes(clazz)
      len = decode(bytes)
      buffer = new PickleBuffer(bytes)
      if checkVersion(buffer)
    } yield {
      val nEntries = buffer.readNat()
      val entries = Array.ofDim[Entry](nEntries)

      trait EntryRefImpl extends EntryRef {
        val table = entries
      }

      if (debug) println("" + nEntries + " entries")
      for (i <- 0 until nEntries) {
        val entryType = buffer.readByte()
        val entryLen = buffer.readNat()
        val start = buffer.readIndex
        val end = buffer.readIndex + entryLen
        def atEnd = buffer.readIndex >= end

        import PickleFormat._

        entries(i) = entryType match {
          case TERMname => TermName(buffer.readString(entryLen))
          case TYPEname => TypeName(buffer.readString(entryLen))
          case NONEsym => NoneSym()
          case TYPEsym => new TypeSym(buffer.readSymbolInfo(entryLen)) with EntryRefImpl
          case ALIASsym => new AliasSym(buffer.readSymbolInfo(entryLen)) with EntryRefImpl
          case CLASSsym =>
            val symbolInfo = buffer.readSymbolInfo(entryLen)
            if (atEnd) new ClassSym(symbolInfo, None) with EntryRefImpl
            else new ClassSym(symbolInfo, Some(buffer.readNat())) with EntryRefImpl
          case MODULEsym => new ModuleSym(buffer.readSymbolInfo(entryLen)) with EntryRefImpl
          case VALsym =>
            val symbolInfo = buffer.readSymbolInfo(entryLen)
            if (atEnd) new ValSym(symbolInfo, None) with EntryRefImpl
            else new ValSym(symbolInfo, Some(buffer.readNat())) with EntryRefImpl
          case EXTref =>
            val nameRef = buffer.readNat()
            if (atEnd) new ExtRef(nameRef, None) with EntryRefImpl
            else new ExtRef(nameRef, Some(buffer.readNat())) with EntryRefImpl
          case EXTMODCLASSref =>
            val nameRef = buffer.readNat()
            if (atEnd) new ExtModClassRef(nameRef, None) with EntryRefImpl
            else new ExtModClassRef(nameRef, Some(buffer.readNat())) with EntryRefImpl
          case NOtpe => NoType()
          case NOPREFIXtpe => NoPrefixType()
          case THIStpe => new ThisType(buffer.readNat()) with EntryRefImpl
          case SINGLEtpe => new SingleType(buffer.readNat(), buffer.readNat()) with EntryRefImpl
          case CONSTANTtpe => ConstantType(buffer.readNat())
          case TYPEREFtpe =>
            val typeRef = buffer.readNat()
            val symRef = buffer.readNat()
            val typeArgs = new ArrayBuffer[Int]()
            while (!atEnd) typeArgs += buffer.readNat()
            new TypeRefType(typeRef, symRef, typeArgs) with EntryRefImpl
          case TYPEBOUNDStpe => new TypeBoundsType(buffer.readNat(), buffer.readNat()) with EntryRefImpl
          case REFINEDtpe =>
            val classSymRef = buffer.readNat()
            val typeRefs = new ArrayBuffer[Int]()
            while (!atEnd) typeRefs += buffer.readNat()
            new RefinedType(classSymRef, typeRefs) with EntryRefImpl
          case CLASSINFOtpe =>
            val classSymRef = buffer.readNat()
            val typeRefs = new ArrayBuffer[Int]()
            while (!atEnd) typeRefs += buffer.readNat()
            new ClassInfoType(classSymRef, typeRefs) with EntryRefImpl
          case METHODtpe =>
            val typeRef = buffer.readNat()
            val symRefs = new ArrayBuffer[Int]()
            while (!atEnd) symRefs += buffer.readNat()
            new MethodType(typeRef, symRefs) with EntryRefImpl
          case POLYtpe =>
            val typeRef = buffer.readNat()
            val symRefs = new ArrayBuffer[Int]()
            while (!atEnd) symRefs += buffer.readNat()
            new PolyType(typeRef, symRefs) with EntryRefImpl
          case SUPERtpe => new SuperType(buffer.readNat(), buffer.readNat()) with EntryRefImpl
          case EXISTENTIALtpe =>
            val typeRef = buffer.readNat()
            val symRefs = new ArrayBuffer[Int]()
            while (!atEnd) symRefs += buffer.readNat()
            new ExistentialType(typeRef, symRefs) with EntryRefImpl
          case CHILDREN =>
            val symRef = buffer.readNat()
            val symRefs = new ArrayBuffer[Int]()
            while (!atEnd) symRefs += buffer.readNat()
            Children(symRef, symRefs)
          case _ =>
            buffer.skip(entryLen)
            TODOEntry(entryType)
        }

        if (buffer.readIndex != end) {
          entries(i) = BrokenEntry(i, entries(i), Arrays.copyOfRange(bytes, start, end))
          buffer.readIndex = end
        }
        if (debug) println("%05d : %s".format(i, entries(i)))

      }
      entries
    }

  }

  private def getSigBytes(clazz: Class[_]) = {
    val sig = clazz.getAnnotation(classOf[ScalaSignature])
    val sigString =
      if (sig == null) {
        val longSig = clazz.getAnnotation(classOf[ScalaLongSignature])
        if (longSig == null) None
        else Some(longSig.bytes mkString)
      } else
        Some(sig.bytes)

    sigString map (_.getBytes("UTF-8"))
  }

  private def min1(src: Array[Byte]) {
    var i = 0
    val srclen = src.length
    while (i < srclen) {
      val in: Int = src(i) & 0xff
      if (in == 0x00) {
        src(i) = 0x7f
      } else {
        src(i) = (in - 1).toByte
      }
      i += 1
    }
  }

  private def decode(src: Array[Byte]) = {
    min1(src)
    ByteCodecs.decode7to8(src, src.length)
  }

  private def checkVersion(buffer: PickleBuffer) = {
    val major = buffer.readNat()
    val minor = buffer.readNat()
    (major == PickleFormat.MajorVersion) && (minor <= PickleFormat.MinorVersion)
  }
}