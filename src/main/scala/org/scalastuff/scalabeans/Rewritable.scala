package org.scalastuff.scalabeans
import org.scalastuff.scalabeans.types.BeanType
import org.scalastuff.scalabeans.types.ScalaType

import org.scalastuff.util.ConcurrentMapMemo._

sealed trait Rewritable[T] {
  def rewrite(rules: Rules[T]): T
}

sealed trait Rules[A] extends PartialFunction[A, A]

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

  def propertyRules(rules: PartialFunction[PropertyDescriptor, PropertyDescriptor]): Rules[PropertyDescriptor] = new Rules[PropertyDescriptor] {    
    def apply(pd: PropertyDescriptor) = applyMemo(pd) {
      //println("rules applied to property " + pd)
      rewritePropertyType(pf2rules(rules)(pd), typeRules)
    }

    def isDefinedAt(pd: PropertyDescriptor) = rules.isDefinedAt(pd) || typeRules.isDefinedAt(pd.scalaType)
    
    private[this] val applyMemo = memo[PropertyDescriptor, PropertyDescriptor]
    private[this] val typeRules: Rules[ScalaType] = this
  }

  implicit def beanRulesFromPropertyRules(propertyRules: Rules[PropertyDescriptor]): Rules[BeanDescriptor] = new Rules[BeanDescriptor] {
    def apply(bd: BeanDescriptor) = applyMemo(bd) {
      //println("rules applied to bean " + bd)
      rewriteBeanProperties(bd, propertyRules)
    }

    def isDefinedAt(bd: BeanDescriptor) = bd.properties.exists(propertyRules.isDefinedAt _)

    private[this] val applyMemo = memo[BeanDescriptor, BeanDescriptor]
  }

  def beanRules(rules: PartialFunction[BeanDescriptor, BeanDescriptor]): Rules[BeanDescriptor] = new Rules[BeanDescriptor] {
    def apply(bd: BeanDescriptor) = applyMemo(bd) {
      rewriteBeanProperties(pf2rules(rules)(bd), propertyRules)
    }

    def isDefinedAt(bd: BeanDescriptor) = rules.isDefinedAt(bd) || bd.properties.exists(propertyRules.isDefinedAt _)

    private[this] val propertyRules = propertyRulesFromTypeRules(typeRulesFromBeanRules(this))
    private[this] val applyMemo = memo[BeanDescriptor, BeanDescriptor]
  }

  def typeRules(rules: PartialFunction[ScalaType, ScalaType]) = new Rules[ScalaType] {    
    def apply(scalaType: ScalaType) = applyMemo(scalaType) {
      val result =
        if (scalaType.arguments.exists(isDefinedAt _))
          ScalaType(scalaType.erasure, scalaType.arguments map apply)
        else
          scalaType

      if (rules.isDefinedAt(result)) rules(result)
      else result
    }

    def isDefinedAt(scalaType: ScalaType) = rules.isDefinedAt(scalaType) || scalaType.arguments.exists(isDefinedAt _)
    
    private[this] val applyMemo = memo[ScalaType, ScalaType]
  }

  implicit def typeRulesFromBeanRules(beanRules: Rules[BeanDescriptor]) = typeRules {
    case bt @ BeanType(bd) if beanRules.isDefinedAt(bd) => bt.copy(bd rewrite beanRules)
  }

  implicit def typeRulesFromPropertyRules(propertyRules: Rules[PropertyDescriptor]): Rules[ScalaType] = {
    typeRulesFromBeanRules(beanRulesFromPropertyRules(propertyRules))
  }

  implicit def propertyRulesFromTypeRules(typeRules: Rules[ScalaType]) = new Rules[PropertyDescriptor] {
    def apply(pd: PropertyDescriptor): PropertyDescriptor = rewritePropertyType(pd, typeRules)
    def isDefinedAt(pd: PropertyDescriptor) = typeRules.isDefinedAt(pd.scalaType)
  }

  private def pf2rules[T](pf: PartialFunction[T, T]) = { x: T =>
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