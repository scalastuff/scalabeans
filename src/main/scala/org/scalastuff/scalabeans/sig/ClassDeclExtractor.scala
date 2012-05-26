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

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import scala.reflect.generic.Flags._
import com.google.common.collect.MapMaker

/**
 * Extracts class and object declaration information from Scala signature.
 *
 * Only top-level classes and objects are parsed now. Declarations contain declared parents,
 * property candidates (no strict filtering, actual properties are still resolved via Java reflection),
 * type aliases. All returned objects are thread-safe.
 *
 * No types in other top-level entities are referenced directly, but via ExternalTermRef,
 * ExternalModuleRef and ExternalTypeRef. Those must be resolved by extracting declarations
 * from corresponding top-level entities.
 *
 * Uses output from UnPickler to get raw data structures in PickleFormat.
 */
object ClassDeclExtractor {
  def extract[T](implicit mf: Manifest[T]): Option[Seq[Mirror.EntityDecl]] = extract(mf.erasure)
  def extract(clazz: String): Option[Seq[Mirror.EntityDecl]] = extract(Class.forName(clazz))

  /**
   * Declarations are grouped by the class where they appear in the Scala signature.
   */
  private[this] val declarationsCache = new MapMaker().softValues().makeMap[Class[_], Option[Seq[Mirror.EntityDecl]]]()

  def extract(clazz: Class[_]): Option[Seq[Mirror.EntityDecl]] = {
    val cached = declarationsCache.get(clazz)
    if (cached != null) return cached

    val extracted =
      for (table <- UnPickler.read(clazz)) yield new ClassDeclExtractor().extract(table)

    declarationsCache.putIfAbsent(clazz, extracted)
    extracted
  }

  val mirror: scala.metaprogramming.Mirror = new scala.metaprogramming.Mirror {
    val rootPackageDecl = new PackageDecl {
      val name = "<root>"
      val owner = this
    }
  }
  
//  type Type = mirror.Type
//  type EntityDecl = mirror.EntityDecl
//  case class ClassDecl
//  case class ValueDecl(name:String, owner: EntityDecl, visibility: Visibility, valueType: Type) extends mirror.ValueDecl with mirror.MemberDecl
//  
//  type Visibility = mirror.Visibility
//  object Public extends mirror.Public
//  
}

/**
 * This class is not thread-safe and must be not used directly. Use companion object API instead.
 */
class ClassDeclExtractor {
  import UnPickler._
  import Mirror._

  def extract(table: Array[Entry]): Seq[EntityDecl] = {
    val children = HashMap[SymbolDeclEntry, ArrayBuffer[SymbolDeclEntry]]()
    val topLevelEntries = ArrayBuffer[SymbolDeclEntry]()

    // collect symbol children
    for (i <- 0 until table.length) table(i) match {
      case symbolEntry: SymbolDeclEntry =>
        symbolEntry.owner match {
          case parent: SymbolDeclEntry =>
            val symbolChildren = children.getOrElseUpdate(parent, new ArrayBuffer[SymbolDeclEntry]())
            symbolChildren += symbolEntry
          case _ if symbolEntry.isInstanceOf[ClassSym] || symbolEntry.isInstanceOf[ModuleSym] =>
            topLevelEntries += symbolEntry
        }
      case _ =>
    }

    // parse top-level entities
    for (symDeclEntry <- topLevelEntries)
      yield symDeclEntry match {
      case cs: ClassSym => parseClassSymbol(cs, children.getOrElse(cs, Seq.empty[SymbolDeclEntry]))
      case ms: ModuleSym => parseModuleSymbol(ms, children.getOrElse(ms, Seq.empty[SymbolDeclEntry]))
    }
  }

  private[this] def extractValues(_owner: EntityDecl, children: Seq[SymbolDeclEntry]): Seq[ValueDecl with MemberDecl] = children collect {
    case vs: ValSym if !(vs.name.endsWith(" "))
      && (vs.typeEntry.isValueType)
      && (vs.flags & (FINAL | SYNTHETIC)) == 0 =>
      new ValueDecl with MemberDecl {
        val name = vs.name
        val owner = _owner
        val visibility = Public // TODO
        val valueType = parseType(vs.typeEntry)
      }
  }

//  private[this] def extractMembers(_owner: EntityDecl, children: Seq[SymbolDeclEntry]): Seq[ValueDecl with MemberDecl] = {
//    val vals = new ArrayBuffer[ValueDecl with MemberDecl]()
//    val methods = new ArrayBuffer[ValueDecl with MemberDecl]()
//    for (member <- children) member match {
//      case vs: ValSym if !(vs.name.endsWith(" "))
//        && (vs.typeEntry.isValueType)
//        && (vs.flags & (FINAL | SYNTHETIC)) == 0 =>
//        new ValueDecl with MemberDecl {
//          val name = vs.name
//          val owner = _owner
//          val visibility = Public // TODO
//          val valueType = parseType(vs.typeEntry)
//        }
//    }
//
//    vals
//  }

  def parseClassSymbol(classSym: ClassSym, children: Seq[SymbolDeclEntry]) = {
    new ClassDecl { self =>
      val name = classSym.name
      val owner = parseSymbol(classSym.owner)
      val typeParameters = classSym.typeEntry match {
        case pt: PolyType => pt.symbolEntries map parseTypeParameter
        case _ => Nil
      }
      val parents = parseClassParents(classSym.typeEntry)
      val thisValue = new ValueDecl {
        val name = "this"
        val owner = self
        val valueType = null // TODO      
      }

      val constructors = null // TODO
      val primaryConstructor = null // TODO
      val values = extractValues(self, children)
      val types = children collect {
        case al: AliasSym =>
          new TypeAliasDecl with MemberDecl {
            val name = al.name
            val owner = self
            val visibility = Public // TODO
            val aliased = parseType(al.typeEntry)
            val typeParameters = Nil // TODO
          }
      }
    }
  }

  def parseModuleSymbol(moduleSym: ModuleSym, children: Seq[SymbolDeclEntry]) = {
    val _parents = parseClassParents(moduleSym.typeEntry)
    new ModuleDecl { self =>
      val name = moduleSym.name
      val owner = parseSymbol(moduleSym.owner)
      val valueType = null // TODO
      val parents = _parents toList
      val thisValue = null // TODO

      val values = extractValues(self, children)
      val types = children collect {
        case al: AliasSym =>
          new TypeAliasDecl with MemberDecl {
            val name = al.name
            val owner = self
            val visibility = Public // TODO
            val aliased = parseType(al.typeEntry)
            val typeParameters = Nil // TODO
          }
      }
    }
  }
  
  import org.scalastuff.util.HashMapMemo._

  val parseClassParents: PartialFunction[TypeEntry, List[Type]] = memoize({
    case ct: ClassInfoType => ct.parentEntries map parseType toList
    case trt: TypeRefType => parseClassParents(trt.symbolEntry.asInstanceOf[SymbolDeclEntry].typeEntry) // TODO: apply type parameters
    case pt: PolyType => parseClassParents(pt.typeEntry)
  })

  val parseTermSymbol: PartialFunction[SymbolEntry, TermDecl] = memoize({
    case er: ExtModClassRef =>
      val owner = (er.ownerOption map parseSymbol) getOrElse rootPackageDecl
      ExternalTermRef(er.name, owner) // package or object, undistinguishable here
    case er: ExtRef =>
      val owner = (er.ownerOption map parseSymbol) getOrElse rootPackageDecl
      ExternalModuleRef(er.name, owner) // object
  })

  val createDeclRef: PartialFunction[SymbolEntry, Declaration] = memoize({
    case symbolEntry: TypeSym => ExternalTypeRef(symbolEntry.name, createDeclRef(symbolEntry.owner))
    case symbolEntry: AliasSym => ExternalTypeRef(symbolEntry.name, createDeclRef(symbolEntry.owner))
    case symbolEntry: ClassSym => ExternalTypeRef(symbolEntry.name, createDeclRef(symbolEntry.owner))
    case symbolEntry: ExtRef => ExternalTypeRef(symbolEntry.name, (symbolEntry.ownerOption map createDeclRef) getOrElse rootPackageDecl)
    case symbolEntry: ModuleSym => ExternalTermRef(symbolEntry.name, createDeclRef(symbolEntry.owner))
    case symbolEntry: ValSym => ExternalTermRef(symbolEntry.name, createDeclRef(symbolEntry.owner))
    case symbolEntry: ExtModClassRef => ExternalTermRef(symbolEntry.name, (symbolEntry.ownerOption map createDeclRef) getOrElse rootPackageDecl)
  })

  val parseTypeParameter: PartialFunction[SymbolEntry, TypeParameterDecl] = memoize({
    case ts: TypeSym =>
      def getTypeBoundsEntry(typeEntry: TypeEntry): TypeBoundsType = typeEntry match {
        case tbt: TypeBoundsType => tbt
        case pt: PolyType => getTypeBoundsEntry(pt.typeEntry)
      }
      val typeBoundsEntry = getTypeBoundsEntry(ts.typeEntry)
      new TypeParameterDecl {
        val name = ts.name
        val owner = (ts.ownerOption map createDeclRef) getOrElse rootPackageDecl
        val typeParameters = Nil // TODO
        val variance =
          if ((ts.flags & COVARIANT) != 0) Covariant
          else if ((ts.flags & CONTRAVARIANT) != 0) Contravariant
          else Invariant
        val upperBound = parseType(typeBoundsEntry.upperTypeEntry)
        val lowerBound = parseType(typeBoundsEntry.lowerTypeEntry)
      }
  })

  val parseTypeSymbol: PartialFunction[SymbolEntry, TypeDecl] = parseTypeParameter orElse memoize({
    def typeOwner(symbolEntry: SymbolEntry) = (symbolEntry.ownerOption map parseSymbol) getOrElse rootPackageDecl
    val parseTypeSymbol: PartialFunction[SymbolEntry, TypeDecl] = {
      case er: ExtRef =>
        ExternalTypeRef(er.name, typeOwner(er))
      case al: AliasSym =>
        new TypeAliasDecl {
          val name = al.name
          val owner = typeOwner(al)
          val aliased = parseType(al.typeEntry)
          val typeParameters = al.typeEntry match {
            case pt: PolyType => pt.symbolEntries map parseTypeParameter
            case _ => Nil
          }
        }
      case cs: ClassSym =>
        ExternalTypeRef(cs.name, parseSymbol(cs.owner))
    }
    parseTypeSymbol
  })

  val parseSymbol: PartialFunction[SymbolEntry, Declaration] = parseTermSymbol orElse parseTypeSymbol

  val parseValueType: PartialFunction[TypeEntry, Type] = memoize({
    case trt: TypeRefType =>
      val baseType =
        new TypeProjection {
          val parent = parseType(trt.typeEntry)
          val member = parseTypeSymbol(trt.symbolEntry)
        }
      if (trt.typeParameterEntries.size == 0) baseType
      else new ParameterizedType {
        val parent = baseType
        val parameters = trt.typeParameterEntries map parseType
      }
    case pt: PolyType => parseType(pt.typeEntry) // are the type parameters always the same as in type entry?
    case tt: ThisType =>
      new SingletonType {
        val path = parsePath(tt.symbolEntry)
      }
    case st: SingleType =>
      new SingletonType {
        val path = parsePath(st.symbolEntry)
      }
    case rt: RefinedType =>
      new MixinType {
        val parents = rt.parentEntries map parseType toList
      }
    case NoPrefixType() => EmptySingletonType
  })
  def parseType(typeEntry: TypeEntry): Type = parseValueType(typeEntry)

  def parsePath(symbolEntry: SymbolEntry): Path = symbolEntry match {
    case er: ExternalSymbolEntry =>
      new MemberPath {
        val value = parseTermSymbol(er)
        val parent = (er.ownerOption map { owner => parsePath(owner) }) getOrElse EmptyPath
      }
    case cs: ClassSym =>
      new ThisPath {
        val thisClass = ExternalTypeRef(cs.name, parseSymbol(cs.owner))
      }
  }
}
