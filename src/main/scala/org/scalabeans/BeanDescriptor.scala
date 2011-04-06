package org.scalabeans

trait BeanDescriptor {
  def beanType: ScalaType

  def properties: Seq[PropertyDescriptor]

  def property(name: String) = properties find (_.name == name)

  def apply(name: String) = property(name).get

  private[this] lazy val builderFactory = new BeanBuilderFactory(this, properties.toList)

  def newBuilder() = builderFactory.newBuilder

  def hasImmutableConstructorParameters: Boolean = {
    properties exists {
      prop => prop.isInstanceOf[ImmutablePropertyDescriptor] && prop.isInstanceOf[ConstructorParameter]
    }
  }

  // TODO: views

  def newInstance(args: AnyRef*): AnyRef = {
    if (args.size < builderFactory.constructorParams.size) {

      val builder = newBuilder()

      for (prop <- builder.constructorParams.iterator take args.size)
        builder.set(prop, args(prop.index))

      builder.result()

    } else {
      builderFactory.constructor.newInstance(args: _*)
    }
  }

  /**
   * Top-level class in super-class hierarchy that is not
   * java.lang.Object
   */
  def topLevelClass: Class[_]

  override def toString = beanType.toString
}