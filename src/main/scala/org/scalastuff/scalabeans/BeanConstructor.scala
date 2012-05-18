package org.scalastuff.scalabeans
import java.lang.reflect.Constructor
import com.thoughtworks.paranamer.BytecodeReadingParanamer

private[scalabeans] trait BeanConstructor {
  /**
   * Creates new bean instance from given parameters values.
   */
  def newInstance(args: Array[AnyRef]): AnyRef

  def arity: Int
}

private[scalabeans] case class ConstructorModel(ctor: Array[AnyRef] => AnyRef, parameters: Seq[ConstructorModel.ParameterModel[Any]]) {
  def indexOfParameter(name: String) = parameters.indexWhere(_._1 == name)
}

private[scalabeans] object ConstructorModel {
  type ParameterModel[+A] = Pair[String, Option[() => A]]
  
  def introspectDefaultConstructor(c: Class[_]) = {
    val constructor: Option[Constructor[_]] = {
      if (c.getConstructors().isEmpty) None
      else Some(c.getConstructors()(0).asInstanceOf[Constructor[_]])
    }

    val paranamer = new BytecodeReadingParanamer

    constructor map { ctor =>
      ctor.setAccessible(true)

      val parameterNames = paranamer.lookupParameterNames(ctor)
      val parameters = (parameterNames, (1 to parameterNames.length)).zipped map { (parameterName, i) =>
        (parameterName, c.getMethods.find(_.getName == "init$default$" + i) map { method => { () => method.invoke(null) } })
      }

      ConstructorModel({ args: Array[AnyRef] => ctor.newInstance(args: _*).asInstanceOf[AnyRef] }, parameters)
    }
  }
} 