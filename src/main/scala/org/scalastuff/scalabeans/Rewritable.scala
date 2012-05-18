package org.scalastuff.scalabeans
import org.scalastuff.scalabeans.types.BeanType
import org.scalastuff.scalabeans.types.ScalaType

sealed trait Rewritable[T] {
  def rewrite(rules: Rules[T]): T
}

sealed trait Rules[A] extends (A => A)

trait Rewritables {
  implicit def property2Schema(pd: PropertyDescriptor): Rewritable[PropertyDescriptor] = new Rewritable[PropertyDescriptor] {
    def rewrite(rules: Rules[PropertyDescriptor]) = rules(pd)
  }

  implicit def bean2Schema(bd: BeanDescriptor): Rewritable[BeanDescriptor] = new Rewritable[BeanDescriptor] {
    def rewrite(rules: Rules[BeanDescriptor]) = rules(bd)
  }

  def propertyRules(rules: PartialFunction[PropertyDescriptor, PropertyDescriptor]): Rules[PropertyDescriptor] = new Rules[PropertyDescriptor] {
    def apply(pd: PropertyDescriptor) = deepBeanRewrite(pf2rules(rules)(pd), _.rewrite(this))
  }

  implicit def beanRules(propertyRules: Rules[PropertyDescriptor]): Rules[BeanDescriptor] = new Rules[BeanDescriptor] {
    def apply(bd: BeanDescriptor) = rewriteProperties(bd, _.rewrite(propertyRules))
  }

  def beanRules(rules: PartialFunction[BeanDescriptor, BeanDescriptor]): Rules[BeanDescriptor] = new Rules[BeanDescriptor] {
    def apply(bd: BeanDescriptor) = rewriteProperties(rules(bd), deepBeanRewrite(_, rules))
  }

  private def pf2rules[T](pf: PartialFunction[T, T]) = { x: T =>
    if (pf.isDefinedAt(x)) pf(x)
    else x
  }

  private def deepBeanRewrite(pd: PropertyDescriptor, rules: BeanDescriptor => BeanDescriptor) = {
    val newType = pd.scalaType.rewrite {
      case bt @ BeanType(bd) => bt.copy(rules(bd))
    }

    if (newType eq pd.scalaType) pd
    else pd.updateScalaType(newType)
  }

  private def rewriteProperties(bd: BeanDescriptor, rules: PropertyDescriptor => PropertyDescriptor) = {
    val newProperties = bd.properties map rules

    if (bd.properties.sameElements(newProperties)) bd
    else bd.updateProperties(newProperties)
  }
}