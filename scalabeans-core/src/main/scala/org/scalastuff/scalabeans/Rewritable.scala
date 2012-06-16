package org.scalastuff.scalabeans
import org.scalastuff.scalabeans.types.BeanType
import org.scalastuff.scalabeans.types.ScalaType

import org.scalastuff.util.ConcurrentMapMemo._

sealed trait Rewritable[T] {
  def rewrite(rules: Rules[T]): T
}

/**
 * Defines rewrite rules for certain type.
 */
sealed trait Rules[A] {
  /**
   * Applies rules to a value.
   * 
   * If no rules are applicable, same value is returned.
   */
  def apply(x: A): A
  
  /**
   * Checks either any rules are defined for a value.
   */
  def isDefinedAt(x: A): Boolean = isDefinedAt(x, Seq[Any]())
  
  protected[scalabeans] def isDefinedAt(x: A, visited: Seq[Any]) : Boolean = {
    if (visited.contains(x)) false
    else {
      // println("Visited " + x)
     _isDefinedAt(x, visited :+ x)
    }
  }
  
  protected def _isDefinedAt(x: A, visited: Seq[Any]): Boolean
  
  /**
   * Composes this rules with fallback rules which are applied if this rules are not applicable.
   */
  def orElse(that: Rules[A]) = new Rules[A] {
    
    def _isDefinedAt(x: A, visited: Seq[Any]) = Rules.this.isDefinedAt(x, visited) || that.isDefinedAt(x, visited)
    
    def apply(x: A) = {
      if (Rules.this.isDefinedAt(x)) Rules.this.apply(x)
      else that.apply(x)
    }
  }
  
  /**
   * Composes this rules with next rules which are applied to the result of application of this rules.
   */
  def andThen(next: Rules[A]) = new Rules[A] {
    def _isDefinedAt(x: A, visited: Seq[Any]) = Rules.this.isDefinedAt(x, visited) || next.isDefinedAt(x, visited)
    
    def apply(x: A) = next.apply(Rules.this.apply(x))
  }
}

sealed abstract class EmptyRules[A] extends Rules[A] {
  def apply(x: A) = x
  def _isDefinedAt(x: A, visited: Seq[Any]) = false
}

trait Rewritables {
  implicit def property2Schema(pd: PropertyDescriptor): Rewritable[PropertyDescriptor] = new Rewritable[PropertyDescriptor] {
    def rewrite(rules: Rules[PropertyDescriptor]) = rules(pd)
  }

  implicit def bean2Schema(bd: BeanDescriptor): Rewritable[BeanDescriptor] = new Rewritable[BeanDescriptor] {
    def rewrite(rules: Rules[BeanDescriptor]) = rules(bd)
  }

  implicit def scalaType2Schema(scalaType: ScalaType): Rewritable[ScalaType] = new Rewritable[ScalaType] {
    def rewrite(rules: Rules[ScalaType]) = rules(scalaType)
  }

  object EmptyScalaTypeRules extends EmptyRules[ScalaType]

  def propertyRules(propertyPf: PartialFunction[PropertyDescriptor, PropertyDescriptor]): Rules[PropertyDescriptor] = new Rules[PropertyDescriptor] {
    def apply(pd: PropertyDescriptor) = applyMemo(pd) {
      // println("rules applied to property " + pd)
      rewritePropertyType(applyPfAsRules(propertyPf, pd), typeRules)
    }

    def _isDefinedAt(pd: PropertyDescriptor, visited: Seq[Any]) = propertyPf.isDefinedAt(pd) || typeRules.isDefinedAt(pd.scalaType, visited)

    private[this] val applyMemo = memo[PropertyDescriptor, PropertyDescriptor]
    private[this] val typeRules: Rules[ScalaType] = typeRulesFromPropertyRules(this)
  }

  implicit def beanRulesFromPropertyRules(propertyRules: Rules[PropertyDescriptor]): Rules[BeanDescriptor] = new Rules[BeanDescriptor] {
    def apply(bd: BeanDescriptor) = applyMemo(bd) {
      // println("rules applied to bean " + bd)
      rewriteBeanProperties(bd, propertyRules)
    }

    def _isDefinedAt(bd: BeanDescriptor, visited: Seq[Any]) = bd.properties.exists(propertyRules.isDefinedAt(_, visited))

    private[this] val applyMemo = memo[BeanDescriptor, BeanDescriptor]
  }

  def beanRules(beanPf: PartialFunction[BeanDescriptor, BeanDescriptor]): Rules[BeanDescriptor] = new Rules[BeanDescriptor] {
    def apply(bd: BeanDescriptor) = applyMemo(bd) {
      rewriteBeanProperties(applyPfAsRules(beanPf, bd), propertyRules)
    }

    def _isDefinedAt(bd: BeanDescriptor, visited: Seq[Any]) = beanPf.isDefinedAt(bd) || bd.properties.exists(propertyRules.isDefinedAt(_, visited))

    private[this] val propertyRules = propertyRulesFromTypeRules(typeRulesFromBeanRules(this))
    private[this] val applyMemo = memo[BeanDescriptor, BeanDescriptor]
  }

  def typeRules(scalaTypePf: PartialFunction[ScalaType, ScalaType]) = new Rules[ScalaType] {
    def apply(scalaType: ScalaType) = applyMemo(scalaType) {
      val result =
        if (scalaType.arguments.exists(isDefinedAt _))
          ScalaType(scalaType.erasure, scalaType.arguments map apply)
        else
          scalaType

      if (scalaTypePf.isDefinedAt(result)) scalaTypePf(result)
      else result
    }

    def _isDefinedAt(scalaType: ScalaType, visited: Seq[Any]) = scalaTypePf.isDefinedAt(scalaType) || scalaType.arguments.exists(isDefinedAt(_, visited))

    private[this] val applyMemo = memo[ScalaType, ScalaType]
  }

  implicit def typeRulesFromBeanRules(beanRules: Rules[BeanDescriptor]) = new Rules[ScalaType] {
    def apply(scalaType: ScalaType) = applyMemo(scalaType) {
      val result =
        if (scalaType.arguments.exists(isDefinedAt _))
          ScalaType(scalaType.erasure, scalaType.arguments map apply)
        else
          scalaType
      
      result match {
        case bt @ BeanType(bd) if beanRules.isDefinedAt(bd) => bt.copy(bd rewrite beanRules)
        case _ => result
      }
    }

    def _isDefinedAt(scalaType: ScalaType, visited: Seq[Any]) = scalaType match {
      case bt @ BeanType(bd) => beanRules.isDefinedAt(bd, visited) || scalaType.arguments.exists(isDefinedAt(_, visited))
      case _ => scalaType.arguments.exists(isDefinedAt(_, visited))
    } 

    private[this] val applyMemo = memo[ScalaType, ScalaType]
  }

  implicit def typeRulesFromPropertyRules(propertyRules: Rules[PropertyDescriptor]): Rules[ScalaType] = {
    typeRulesFromBeanRules(beanRulesFromPropertyRules(propertyRules))
  }

  implicit def propertyRulesFromTypeRules(typeRules: Rules[ScalaType]) = new Rules[PropertyDescriptor] {
    def apply(pd: PropertyDescriptor): PropertyDescriptor = rewritePropertyType(pd, typeRules)
    def _isDefinedAt(pd: PropertyDescriptor, visited: Seq[Any]) = typeRules.isDefinedAt(pd.scalaType, visited)
  }

  private def applyPfAsRules[T](pf: PartialFunction[T, T], x: T): T = {
    if (pf.isDefinedAt(x)) pf(x)
    else x
  }

  private def rewritePropertyType(pd: PropertyDescriptor, typeRules: Rules[ScalaType]): PropertyDescriptor = {
    if (typeRules.isDefinedAt(pd.scalaType))
      pd.updateScalaType(pd.scalaType rewrite typeRules)
    else
      pd
  }

  private def rewriteBeanProperties(bd: BeanDescriptor, rules: Rules[PropertyDescriptor]): BeanDescriptor = {
    if (bd.properties.exists(rules.isDefinedAt _)) {
      val newProperties = bd.properties map (_.rewrite(rules))
      bd.updateProperties(newProperties)
    } else
      bd
  }
}