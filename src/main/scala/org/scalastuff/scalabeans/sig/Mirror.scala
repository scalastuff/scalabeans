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

import collection._

object Mirror extends Mirror

/** A mirror is the entry point of Scala's meta-programming infrastructure: it reflects program entities such as
  * instances, classes, etc. Every program entity is reflected as a unique `Reflection` instance in this mirror.
  *
  * A mirror only reflects program entities that are within its field of view, which interpretation is left to
  * subclasses (usually a given virtual machine and class loader, or a compiler instance).
  *
  * Note that the `Reflection` class and its subclasses are path-dependent to the mirror that owns them. */
trait Mirror { mirror =>
    
  case class ExternalTermRef(name: String, owner: Declaration) extends TermDecl
  case class ExternalModuleRef(name: String, owner: Declaration) extends TermDecl
  case class ExternalTypeRef(name: String, owner: Declaration) extends TypeDecl {
    val typeParameters: TypeParameters = Nil // real type parameters have to be resolved from actual declaration
    override def toString = owner.qualifiedName + "." + name
  }
//  case class ExternalAliasTypeRef(name: String, owner: Declaration) extends TypeDecl {
//    val typeParameters: TypeParameters = Nil // real type parameters have to be resolved from actual declaration
//    override def toString = owner.qualifiedName + "#" + name
//  }

  type Name = String  

  /** A reflection represents a program entity such as an instance, class, etc. reflected in the mirror owning it. */
  trait Reflection

  /* ========== TYPE ========== */

  /** A type (ref. §3), which is the description of a set of permissible values. Every value and most declarations
    * have a type. Reflection types are full Scala types, not aproximated "erased" types (ref. §3.7), which this
    * framework represents as `java.lang.Class` instances. */
  trait Type extends Reflection

  /** A singleton type (ref. §3.2.1) of the form `<path>.type`. */
  trait SingletonType extends Type {
    val path: Path
    override def toString =
      path.toString + ".type"
  }
  object EmptySingletonType extends SingletonType {
    val path = EmptyPath
  }

  /** A type projection (ref. §3.2.2) of the form `<parent>#<member>`. Note that a reference to a type variable `T` is
    * represented as `ε.type#T`, and that a type in dot notation like `scala.Int` is represented as `scala.type#Int.` */
  trait TypeProjection extends Type {
    def parent: Type
    def member: TypeDecl //with MemberDecl
    override def toString = parent match {
      case st: SingletonType =>
        st.path match {
          case mp: MemberPath =>
            if (mp.value.isInstanceOf[ExternalModuleRef]) mp.toString + "#" + member.toString
            else member.toString
          case _ => parent.toString + "#" + member.toString
        }
      case _ => parent.toString + "#" + member.toString        
    }
  }

  /** A parameterized type (ref. §3.2.4) of the form `<parent>[<parameter1>, ...]`. */
  trait ParameterizedType extends Type {
    def parent: Type
    def parameters: Seq[Type]
    override def toString =
      parent.toString + (parameters map (_.toString)).mkString("[", ", ", "]")
  }

  /** A type refinement of the form `<parent> { <declaration1> ; ... }`, part of a compound type (ref. §3.2.7). */
  trait TypeRefinement extends Type {
    def parent: Type
    def declarations: List[MemberDecl]
    override def toString =
      parent.toString + (declarations map (_.toString)).mkString("{", "; ", "}")
  }

  /** A mixin type of the form `<parent1> with ...`, part of a compound type (ref. §3.2.7). */
  trait MixinType extends Type {
    def parents: List[Type]
    override def toString =
      (parents map (_.toString)).mkString(" with ")
  }

  /** An existential type (ref. §3.2.10) of the form `<template> forSome { <constraint1> ; ... }`. */
  trait ExistentialType extends Type {
    def template: Type
    def constraints: List[AbstractTypeDecl]
    override def toString =
      template.toString + " forSome " + (constraints map (_.toString)).mkString("{", "; ", "}")
  }

  /* ========== PATH ========== */

  /** A path is a component of a type that defines which value owns a given type member (ref. §3.1). */
  trait Path extends Reflection

  /** The empty path `ε`. */
  object EmptyPath extends Path {
    override def toString =
      "ε"
  }

  /** A stable member-value path of the form `<parent>.<value>`. The selected member-value may be the declaration of
    * an object, package or value. */
  trait MemberPath extends Path {
    def parent: Path
    def value: TermDecl // package or object or value of non-volatile type
    override def toString =
      if (parent == EmptyPath) value.name
      else parent.toString + "." + value.name
  }

  /** A this-reference path of the form `<thisClass>.this`. */
  trait ThisPath extends Path {
    def thisClass: TypeDecl // circular dependency: EntityDecl
    override def toString =
      thisClass.qualifiedName + ".this"
  }

  /** A super-reference path of the form `<thisClass>.super[<superClass>]`. */
  trait SuperPath extends Path {
    def thisClass: EntityDecl
    def superClass: ClassOrTraitDecl
    override def toString =
      thisClass.qualifiedName + ".super[" + superClass.name + "]"
  }

  /* ========== DECLARATION ========== */

  type TypeParameters = Seq[TypeParameterDecl]
  type ValueParameters = Seq[Seq[ValueParameterDecl]]

  /** A declaration, which is the introduction of a new symbol, such as a new class, class member, parameter, etc. */
  trait Declaration extends Reflection {
    def owner: Declaration
    def name: Name
    def qualifiedName: String =
      if (this == rootPackageDecl) name
      else if (owner == rootPackageDecl) name
      else owner.qualifiedName + "." + name
  }

  /** A declaration of a new member of a class or object. This trait will be mixed into a declaration only if it is a
    * member that can be accessed from the owner (including privately). Anonymous inner classes, inner methods or
    * values, and the likes are not member declarations. */
  trait MemberDecl extends Declaration {
    def owner: EntityDecl
    def visibility: Visibility
  }

  /** A declaration of a new type member of a class or object. This trait will be mixed into a type declaration only
    * if it can be projected from the owner class. */
  trait TypeDecl extends Declaration {
    def typeParameters: TypeParameters
  }

  /**  */
  trait TypeAliasDecl extends TypeDecl { // Must mixin either MemberDecl or Statement
    def aliased: Type
    override def toString =
      "type " + name + (typeParameters map (_.toString)).mkString("[", ", ", "]") + " = " + aliased.toString
  }

  /**  */
  trait AbstractTypeDecl extends TypeDecl { // Must mixin MemberDecl
    def lowerBound: Type
    def upperBound: Type
    override def toString: String =
      "type " + name + (typeParameters map (_.toString)).mkString("[", ", ", "]") + " >: " + lowerBound.toString + " <: " + upperBound.toString
  }

  /** A declaration of a type parameter. Classes and methods take such parameters. */
  trait TypeParameterDecl extends AbstractTypeDecl {
    def variance: Variance
    def typeParameters: TypeParameters
    override def toString = {      
        if (lowerBound == upperBound) upperBound.toString
        else {
          val typeParamsStr = 
	        if (typeParameters.size == 0) ""
	        else typeParameters.map(_.toString).mkString("[", ", ", "]")
            
          variance.toString + name + typeParamsStr + " >: " + lowerBound.toString + " <: " + upperBound.toString
        }
    }
  }

  /** Object, Class or Trait */
  trait EntityDecl extends Declaration {
    def parents: List[Type]
    def thisValue: ValueDecl
    def types: Seq[TypeAliasDecl with MemberDecl]
    // TODO: def terms: OverloadedScope[TermDecl with MemberDecl]
    // TODO: def methods: OverloadedScope[MethodDecl with MemberDecl]
    def values: Seq[ValueDecl with MemberDecl]
    // TODO: def classes: Scope[ClassDecl with MemberDecl]
    // TODO: def modules: Scope[ModuleDecl with MemberDecl]
    //def packages: Scope[PackageDecl]
  }

  trait ClassOrTraitDecl extends TypeDecl with EntityDecl {
    def typeParameters: TypeParameters
  }

  /**  */
  trait ClassDecl extends ClassOrTraitDecl { // Must mixin either MemberDecl or Statement    
    def constructors: OverloadedDecls[ConstructorDecl]
    def primaryConstructor: ConstructorDecl
    override def toString =
      "class " + qualifiedName + /*(typeParameters map (_.toString)).mkString("[", ", ", "]") +
              (primaryConstructor.parameters map (_.toString)).mkString("(", ", ", ")")*/ // TODO comment
              (parents map (_.toString)).mkString(" extends ", " with ", "") +
              " {\n" + (types.map(_.toString).mkString("", "\n", "\n")) + 
               (values.map(_.toString).mkString("", "\n", "\n")) + "}"
  }

  trait TraitDecl extends ClassOrTraitDecl { // Must mixin either MemberDecl or Statement
    override def toString =
      "trait " + name + (typeParameters map (_.toString)).mkString("[", ", ", "]") +
              (parents map (_.toString)).mkString(" extends ", " with ", "") +
              "{ " + thisValue.name + ": " + thisValue.valueType.toString + " => }"
  }

  trait TermDecl extends Declaration {}

  /**  */
  trait ConstructorDecl extends TermDecl {
    def owner: EntityDecl
    def parameters: ValueParameters
    override def toString =
      "def this " + (parameters map (_.toString)).mkString("(", ", ", ")")
  }

  /**  */
  trait MethodDecl extends TermDecl { // Must mixin either MemberDecl or Statement
    def resultType: Type
    def typeParameters: TypeParameters
    def parameters: ValueParameters
    override def toString =
      "def " + name + (typeParameters map (_.toString)).mkString("[", ", ", "]") +
              (parameters map (_.toString)).mkString("(", ", ", ")") + ": " + resultType
  }

  /**  */
  trait ValueDecl extends TermDecl { // Must mixin either MemberDecl or Statement
    def valueType: Type
    override def toString =
      "val " + name + ": " + valueType.toString
  }

  /**  */
  trait LazyValueDecl extends ValueDecl { // Must mixin either MemberDecl or Statement
    override def toString =
      "lazy val " + name + ": " + valueType.toString
  }

  /**  */
  trait VariableDecl extends ValueDecl { // Must mixin either MemberDecl or Statement
    override def toString =
      "var " + name + ": " + valueType.toString
  }

  /**  */
  trait ValueParameterDecl extends ValueDecl {
    override def toString =
      name + ": " + valueType.toString
  }

  /**  */
  trait ModuleDecl extends ValueDecl with EntityDecl  { // Must mixin either MemberDecl or Statement
    override def toString =
      "object " + name + (parents map (_.toString)).mkString(" extends ", " with ", "") +
              "{ " + thisValue.name + ": " + thisValue.valueType.toString + " => }"
  }

  /**  */
  trait PackageDecl extends TermDecl {
    def owner: PackageDecl    
    override def toString =
      "package " + name
  }

  val rootPackageDecl = new PackageDecl {
    val name = "<root>"
    val owner = this
  }

  /* ========== SCOPES ========== */

  type ParameterTypes = Seq[Seq[Type]]

  trait Scope[+T <: Declaration] {
    def sequence: Seq[T]
    def apply(name: Name): Option[T]
    override def toString =
      this.sequence.mkString("{ ", ", ", " }")
  }

  trait OverloadedScope[+T <: Declaration] {
    def sequence: Seq[T]
    def alternatives(name: Name): OverloadedDecls[T]
    def apply(name: Name, parametersTypes: ParameterTypes): Option[T]
    override def toString =
      this.sequence.mkString("{ ", ", ", " }")
  }

  trait OverloadedDecls[+T <: Declaration] {
    def sequence: Seq[T]
    def apply(parametersTypes: ParameterTypes): Option[T]
  }

  /* ========== EXPRESSION ========== */

  /**  */
  trait Statement extends Reflection

  /* ========== VARIANCE ========== */

  /** A variance annotation for a type variable (ref. §4.5). */
  sealed trait Variance {
    override def toString = this match {
      case Covariant => "+"
      case Invariant => ""
      case Contravariant => "-"
    }
  }

  /** The variance annotation for a covariant type variable. */
  object Covariant extends Variance

  /** The variance annotation for a contra-variant type variable. */
  object Contravariant extends Variance

  /** The variance annotation for an invariant type variable. */
  object Invariant extends Variance

  /* ========== VISIBILITY ========== */

  /** A visibility annotation for a member. */
  sealed trait Visibility

  /** */
  object Public extends Visibility

  /** */
  class Protected(qualifier: EntityDecl) extends Visibility

  /** */
  class ProtectedThis extends Visibility

  /** */
  class Private(qualifier: EntityDecl) extends Visibility

  /** */
  class PrivateThis extends Visibility

  /* ========== VALUE ========== */

  /**  */
  trait Value extends Reflection {
    def value: Any
  }

  trait ValueValue extends Value {
    def value: AnyVal
  }

  trait UnitValue extends ValueValue {
    def value: Unit
  }

  trait BooleanValue extends ValueValue {
    def value: Boolean
  }

  trait CharValue extends ValueValue {
    def value: Char
  }

  trait ByteValue extends ValueValue {
    def value: Byte
  }

  trait ShortValue extends ValueValue {
    def value: Short
  }

  trait IntValue extends ValueValue {
    def value: Int
  }

  trait LongValue extends ValueValue {
    def value: Long
  }

  trait FloatValue extends ValueValue {
    def value: Float
  }

  trait DoubleValue extends ValueValue {
    def value: Double
  }

  trait ReferenceValue extends Value {
    def value: AnyRef
  }

  trait StringValue extends ReferenceValue {
    def value: String
  }

  trait TupleValue extends ReferenceValue

  trait FunctionValue extends ReferenceValue

  trait ArrayValue extends ReferenceValue

  trait NullValue extends ReferenceValue {
    def value: Null
  }

  /* ========== ANNOTATION ========== */

  /**  */
  trait Annotation extends Reflection

}