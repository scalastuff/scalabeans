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

import org.scalastuff.scalabeans.types.ScalaType
import org.scalastuff.scalabeans.ManifestFactory
import scala.collection.mutable.ListBuffer
import com.google.common.collect.MapMaker
import org.scalastuff.scalabeans.Preamble._
import Mirror._

/**
 * Compiles ScalaTypes from class declarations provided by ClassDeclExtractor.
 */
object ScalaTypeCompiler {

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
        top <- ClassDeclExtractor.extract(scalaType.erasure)
        classDecl <- top.headOption
        if classDecl.isInstanceOf[ClassDecl]
      } yield {
        new ScalaTypeCompiler(scalaType, classDecl.asInstanceOf[ClassDecl]).compile
      }

    classInfoCache.putIfAbsent(scalaType, extracted)

    extracted
  }

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

  def classForName(clazz: String): Option[Class[_]] = try {
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
    case _ =>
      //throw new IllegalArgumentException("Cannot find class for name: '%s'".format(clazz))
      //println("Cannot find class for name: '%s'".format(clazz))
      None
  }
}

class ScalaTypeCompiler(scalaType: ScalaType, classDecl: ClassDecl) {
  import ScalaTypeCompiler._
  
  val typeParameterBindings = {
    var i = 0
    val map = Map.newBuilder[TypeParameterDecl, Manifest[_]]
    while (i < classDecl.typeParameters.size) {
      map += (classDecl.typeParameters(i) -> ManifestFactory.manifestOf(scalaType.arguments(i)))
      i += 1
    }
    map.result
  }

  def compile: ClassInfo = {
    val parents = classDecl.parents map { p => scalaTypeOf(resolveScalaType(p)) }

    val properties =
      for (member <- classDecl.values) yield {
        (member.name -> scalaTypeOf(resolveScalaType(member.valueType)))
      }

    new ClassInfo(scalaType, parents, Map(properties: _*))
  }

  def resolveScalaType(t: Type): Manifest[_] = {
    def getTypeDecl(t: Type): TypeDecl = t match {
      case tp: TypeProjection =>
        tp.parent match {
          case st: SingletonType =>
            st.path match {
              case mp: MemberPath =>
                if (mp.value.isInstanceOf[ExternalModuleRef]) resolveTypeAlias(mp, tp.member)
                else tp.member
              case _ => tp.member
            }
        }
    }

    t match {
      case tp: TypeProjection => resolveScalaType(getTypeDecl(tp), Nil)
      case pt: ParameterizedType => resolveScalaType(getTypeDecl(pt.parent), pt.parameters)
    }
  }

  def resolveScalaTypeWithParams(aType: Type, typeParameters: Seq[Type]): Manifest[_] = aType match {
    case pt: ParameterizedType =>
      require(pt.parameters.size == typeParameters.size, "Cannot resolve parameterized type %s : parameter count doesn't match".format(aType.toString))
      resolveScalaType(new ParameterizedType {
        val parent = pt.parent
        val parameters = typeParameters // well, we have to apply them here. for now we assume that the order is the same on both sides
      })
    case aliased @ _ =>
      require(typeParameters.size == 0)
      resolveScalaType(aliased)
  }

  def resolveScalaType(td: TypeDecl, typeParameters: Seq[Type]): Manifest[_] = td match {
    case ref: ExternalTypeRef =>
      classForName(ref.qualifiedName) map { erasure =>
        ManifestFactory.manifestOf(erasure, typeParameters map resolveScalaType)
      } getOrElse manifest[Any]
    case alias: TypeAliasDecl => resolveScalaTypeWithParams(alias.aliased, typeParameters)
    case targ: TypeParameterDecl =>
      typeParameterBindings.getOrElse(targ, resolveScalaTypeWithParams(targ.upperBound, typeParameters))
  }

  def resolveTypeAlias(p: Path, typeAlias: TypeDecl): TypeDecl = {
    for {
      erasure <- classForName(p.toString)
      tops <- ClassDeclExtractor.extract(erasure)
      top <- tops
      alias <- top.types find (_.name == typeAlias.name)
    } return alias

    throw new RuntimeException("Cannot resolve type alias %s in %s".format(typeAlias.name, p.toString))
  }
}