package org.scalastuff.scalabeans
import org.scalastuff.util.Rules

trait MetamodelRules {

  def metamodelRules(pf: PartialFunction[Metamodel, Metamodel]): Rules[Metamodel] = Rules(pf)

  def propertyRules(pf: PartialFunction[PropertyDescriptor, PropertyDescriptor]): Rules[Metamodel] = {
    val propertyRules = Rules(pf)
    Rules {
      case bd: BeanDescriptor =>
        bd.updateProperties(bd.properties.map { pd =>
          val updatedProperty = propertyRules(pd)
          require(pd.beanType == updatedProperty.beanType, "Cannot update property: bean types do not match")
          updatedProperty // metamodel of this PD will be updated in metamodel.rewrite(..)
        })
    }
  }

  object EmptyMetamodelRules extends Rules[Metamodel] {
    def isDefinedAt(mm: Metamodel) = false
    def apply(mm: Metamodel) = mm
  }
}