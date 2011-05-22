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
package org.scalastuff.scalabeans

import scala.tools.scalap.scalax.rules.scalasig.ScalaSigParser
import scala.tools.scalap.scalax.rules.scalasig.MethodSymbol
import scala.tools.scalap.scalax.rules.scalasig.{ Type => SigType }
import scala.tools.scalap.scalax.rules.scalasig.TypeRefType
import scala.tools.scalap.scalax.rules.scalasig.MethodType
import scala.tools.scalap.scalax.rules.scalasig.NullaryMethodType
import scala.tools.scalap.scalax.rules.scalasig.{ Symbol => SigSymbol }
import scala.tools.scalap.scalax.rules.scalasig.ThisType
import scala.tools.scalap.scalax.rules.scalasig.SingleType
import scala.tools.scalap.scalax.rules.scalasig.AliasSymbol
import scala.tools.scalap.scalax.rules.scalasig.PolyType
import scala.tools.scalap.scalax.rules.scalasig.ExternalSymbol
import scala.tools.scalap.scalax.rules.scalasig.ScalaSig
import scala.tools.scalap.scalax.rules.scalasig.ClassInfoType
import scala.tools.scalap.scalax.rules.scalasig.AnnotatedType
import scala.tools.scalap.scalax.rules.scalasig.AnnotatedWithSelfType
import scala.collection.mutable.ListBuffer
import com.google.common.collect.MapMaker
import com.google.common.base.{ Function => GFunction }
import org.scalastuff.scalabeans.types.ScalaType

/**
 * Parser of symtables produced by ScalaSigParser, used to find value types of the properties.
 *
 * Doesn't support type parameters and type aliases. Falls back to Any type if cannot resolve the type.
 * Designed to be thread-safe even if ScalaSigParser is not
 */
object SymtableParser {

  import Preamble._

  // is it a good idea to integrate this with ScalaType? like BeanType?
  case class ClassInfo(scalaType: ScalaType, parents: Seq[ScalaType], declaredProperties: Map[String, ScalaType]) {
    /**
     * Linearization of the class.
     *
     * Used to find property types
     */
    lazy val linearization: List[ClassInfo] = {
      parents match {
        case Nil => this :: Nil
        case superClass :: tail =>
          def getLinearizationOf(scalaType: ScalaType) = classInfoOf(scalaType) map (_.linearization) getOrElse Nil

          val linBuffer = new ListBuffer[ClassInfo]
          linBuffer ++= getLinearizationOf(superClass)
          for {
            t <- tail
            lt <- getLinearizationOf(t).reverse
            if !linBuffer.exists(_.scalaType == lt.scalaType)
          } {
            lt +=: linBuffer
          }

          this +=: linBuffer

          linBuffer.result
      }
    }

    /**
     * Searches first properties declared by this class, then goes up via linearization
     */
    def findPropertyType(propertyName: String): Option[ScalaType] = {
      declaredProperties.get(propertyName) match {
        case p: Some[ScalaType] => return p
        case None =>
          for (parent <- linearization) parent.declaredProperties.get(propertyName) match {
            case p: Some[ScalaType] => return p
            case None => ()
          }

          None
      }
    }
  }
  //object ScalaObjectValueType extends ClassInfo(Manifest.Any, Seq.empty, Map.empty)

  private[this] val classInfoCache = new MapMaker().weakKeys().makeMap[ScalaType, Option[ClassInfo]]()

  /**
   * Parses Scala signature to collect property return types.
   */
  def classInfoOf(clazz: String): Option[ClassInfo] = classForName(clazz).flatMap(classInfoOf)

  def classInfoOf(clazz: Class[_]): Option[ClassInfo] = classInfoOf(ManifestFactory.manifestOf(clazz))

  def classInfoOf[A](implicit mf: Manifest[A]): Option[ClassInfo] = classInfoOf(scalaTypeOf(mf))

  def classInfoOf(scalaType: ScalaType): Option[ClassInfo] = {
    val cached = classInfoCache.get(scalaType)
    if (cached != null) return cached

    val extracted =
      for {
        scalaSig <- ScalaSigParser.parse(scalaType.erasure)
        classSym <- scalaSig.topLevelClasses.headOption
      } yield {
        val parents = resolveParents(classSym.infoType)

        val properties =
          for {
            member <- classSym.children
            if !member.name.endsWith(" ")
            propType <- resolvePropertyType(member)
          } yield {
            (member.name -> scalaTypeOf(propType))
          }

        new ClassInfo(scalaType, parents, Map(properties: _*))
      }

    classInfoCache.putIfAbsent(scalaType, extracted)

    extracted
  }

  def resolveParents(t: SigType): Seq[ScalaType] = t match {
    case ClassInfoType(_, typeRefs) => typeRefs map { typeParam => scalaTypeOf(resolveValueType(typeParam)) }
    case PolyType(typeRef, symbols) => resolveParents(typeRef)
    case AnnotatedType(typeRef, _) => resolveParents(typeRef)
    case AnnotatedWithSelfType(typeRef, _, _) => resolveParents(typeRef)
  }

  /**
   * If given member is a property, returns Some(Manifest), None if not a property
   */
  def resolvePropertyType(memberSymbol: SigSymbol): Option[Manifest[Any]] = memberSymbol match {
    case m: MethodSymbol =>
      m.infoType match {
        case NullaryMethodType(resultType) => Some(resolveValueType(resultType))
        case ttt: TypeRefType => Some(resolveValueType(ttt))
        case _ => None
      }
    case _ => None
  }

  /**
   * Tries to resolve value type.
   *
   * If cannot resolve, returns Manifest.Any - this is needed for type parameters
   */
  def resolveValueType(t: SigType): Manifest[Any] = {
    t match {
      case TypeRefType(ThisType(ExternalSymbol(_, _, _)), symbol @ ExternalSymbol(_, _, _), typeArgs) =>
        classForName(symbol.path) map { erasure =>
          val args = typeArgs map resolveValueType
          ManifestFactory.manifestOf(erasure, args).asInstanceOf[Manifest[Any]]
        } getOrElse Manifest.Any

      case TypeRefType(SingleType(_, owner), symbol, typeArgs) =>
        resolveTypeAlias(owner.path, symbol.name) map { mf =>
          val args = typeArgs map resolveValueType
          ManifestFactory.manifestOf(mf.erasure, args).asInstanceOf[Manifest[Any]]
        } getOrElse Manifest.Any
      case PolyType(typeRef, symbols) => resolveValueType(typeRef) // TODO: type param bindings
      case NullaryMethodType(resultType) => resolveValueType(resultType)
      case _ =>
        //println("Unresolved: " + t)
        Manifest.Any
    }
  }

  /**
   * Resolves type aliases defined in singleton types.
   */
  def resolveTypeAlias(singleton: String, typeAlias: String): Option[Manifest[Any]] = {
    for {
      ownerClass <- classForName(singleton)
      resolvedType <- typeAliasesCache.get(ownerClass).get(typeAlias)
    } yield resolvedType
  }

  /**
   * A lot of type aliases defined in scala.package and scala.Predefine, they often appear in the symtables.
   * This Map caches them.
   */
  private[this] val typeAliasesCache = new MapMaker().weakKeys().softValues().makeComputingMap[Class[_], Map[String, Manifest[Any]]](
    new GFunction[Class[_], Map[String, Manifest[Any]]] {
      def apply(clazz: Class[_]) = {
        val typeAliases =
          for {
            scalaSig <- ScalaSigParser.parse(clazz)
          } yield {
            scalaSig.symbols.collect {
              case as: AliasSymbol => (as.name -> resolveValueType(as.infoType))
            }
          }

        Map(typeAliases getOrElse Nil: _*)

      }
    })

  def classForName(clazz: String): Option[Class[_]] = try {
    //println(clazz)
    if (clazz == "scala.Byte") Some(classOf[Byte])
    else if (clazz == "scala.Short") Some(classOf[Short])
    else if (clazz == "scala.Int") Some(classOf[Int])
    else if (clazz == "scala.Long") Some(classOf[Long])
    else if (clazz == "scala.Float") Some(classOf[Float])
    else if (clazz == "scala.Double") Some(classOf[Double])
    else if (clazz == "scala.Char") Some(classOf[Char])
    else if (clazz == "scala.Boolean") Some(classOf[Boolean])
    else if (clazz == "scala.Any") Some(classOf[Any])
    else if (clazz == "scala.AnyVal") Some(classOf[AnyVal])
    else if (clazz == "scala.Unit") Some(classOf[Unit])
    else if (clazz == "scala.Null") Some(classOf[Null])
    else if (clazz == "scala.Nothing") Some(classOf[Nothing])
    else Some(Class.forName(clazz))
  } catch {
    case _ => None
  }
}