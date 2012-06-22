package org.scalastuff.scalabeans.testmono
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

trait RecursiveMetamodelBehaviours extends MonoBehaviours with ShouldMatchers { self: FlatSpec =>

  import resursivemodels._

  def inverseForRecursiveMetamodels() {
    it should "support recursive metamodels" in {
      checkInverse(Person(), Person().set1(), Person().set2)()
    }
  }
}

package resursivemodels {
  case class Person(manager: Option[Person] = None, var team: List[Person] = Nil) {
    def set1() = {
      val manager = Person()
      val teamLid1 = Person(Some(manager))
      val teamLid2 = Person(Some(manager))
      val teamLid3 = Person(Some(manager))

      teamLid3.team = List(teamLid1, teamLid2)

      teamLid3
    }
    
    def set2() = {
      val manager1 = Person()
      val manager2 = Person(Some(manager1))
      val teamLid1 = Person(Some(manager2))
      val teamLid2 = Person(Some(manager2))

      teamLid2.team = List(teamLid1)

      teamLid2
    }
  }
}