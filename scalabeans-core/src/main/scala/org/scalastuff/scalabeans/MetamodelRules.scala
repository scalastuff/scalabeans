package org.scalastuff.scalabeans
import org.scalastuff.util.Rules

trait MetaModelRules {

  def metamodelRules(pf: PartialFunction[MetaModel, MetaModel]): Rules[MetaModel] = Rules(pf)

  def propertyRules(pf: PartialFunction[PropertyDescriptor, PropertyDescriptor]): Rules[MetaModel] = 
    metamodelRules {
      case bd: BeanDescriptor => bd.mapProperties(pf)
    }

  object EmptyMetaModelRules extends Rules[MetaModel] {
    def isDefinedAt(mm: MetaModel) = false
    def apply(mm: MetaModel) = mm
  }
}