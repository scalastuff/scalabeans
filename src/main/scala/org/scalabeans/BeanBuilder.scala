package org.scalabeans

import java.lang.reflect.Constructor
import collection.mutable.{Buffer, BitSet}

// TODO: replace BitSet with ArrayBuffer[Boolean]

abstract class BeanBuilder {
  def beanDescriptor: BeanDescriptor
  def constructor: Constructor[AnyRef]

  def constructorParams: List[ConstructorParameter]
  def mutableProperties: List[MutablePropertyDescriptor]

  def lastConstructorParamIndex: Int
  def lastMutablePropertyIndex: Int

  private[this] val constructorParamValues = Array.ofDim[AnyRef](lastConstructorParamIndex + 1)
  protected val unsetConstructorParams: BitSet

  private[this] val mutablePropertyValues = Array.ofDim[Any](lastMutablePropertyIndex + 1)
  protected val unsetMutableProperties: BitSet

  def unsetProperties(): List[DeserializablePropertyDescriptor] = {
    (constructorParams filter { prop => unsetConstructorParams.contains(prop.index) })  :::
    (mutableProperties filter { prop => unsetMutableProperties.contains(prop.index) })
  }

  def isSet(property: PropertyDescriptor): Boolean = property match {
    case cp: ConstructorParameter => unsetConstructorParams.contains(cp.index)
    case mp: MutablePropertyDescriptor => unsetMutableProperties.contains(mp.index)
    case _ => error("Cannot check if property is set: only constructor parameters and mutable properties are accepted")
  }

  def set(property: PropertyDescriptor, value: Any):Unit = property match {
    case cp: ConstructorParameter =>
      constructorParamValues(cp.index) = value.asInstanceOf[AnyRef]
      unsetConstructorParams -= cp.index
    case mp: MutablePropertyDescriptor =>
      mutablePropertyValues(mp.index) = value
      unsetMutableProperties -= mp.index
    case _ => error("Cannot set property value: only constructor parameters and mutable properties are accepted")
  }

  def get[A](property: PropertyDescriptor):A = property match {
    case cp: ConstructorParameter => constructorParamValues(cp.index).asInstanceOf[A]
    case mp: MutablePropertyDescriptor => mutablePropertyValues(mp.index).asInstanceOf[A]
    case _ => error("Cannot get property value: only constructor parameters and mutable properties are accepted")
  }

  def result() = {
    def reprortMissingConstructorParameters():Nothing = {
      val missingParameterNames =
        for {index <- unsetConstructorParams
             param <- constructorParams find (_.index == index)
             if param.defaultValue.isEmpty
        } yield param.name

      error("Cannot instantiate object of class %s : missing constructor parameters %s".
        format(beanDescriptor.toString, missingParameterNames mkString ", "))
    }

    unsetConstructorParams foreach { index =>
      val constructorParam = constructorParams find (_.index == index) get
      val defaultValue = constructorParam.defaultValue getOrElse reprortMissingConstructorParameters()
      constructorParamValues(index) = defaultValue().asInstanceOf[AnyRef]
    }

    val instance = constructor.newInstance(constructorParamValues: _*)

    mutableProperties withFilter { prop => !unsetMutableProperties.contains(prop.index) } foreach { prop =>
      prop.set(instance, mutablePropertyValues(prop.index))
    }

    instance
  }
}

class BeanBuilderFactory(val beanDescriptor: BeanDescriptor, properties: List[PropertyDescriptor]) {
  val constructor: Constructor[AnyRef] = {
    val c = beanDescriptor.beanType.erasure
    require(c.getConstructors().size > 0,
      "Cannot create BeanBuilderFactory for %s: it has no constructors (either abstract class or interface)".
        format(beanDescriptor.toString))

    c.getConstructors()(0).asInstanceOf[Constructor[AnyRef]]
  }

  constructor.setAccessible(true)

  /**
   * All constructor properties of BeanDescriptor sorted by index, not limited by view
   */
  val constructorParams = {
    val constructorParameters = beanDescriptor.properties collect {
      case p: ConstructorParameter => p
    }

    if (!constructorParameters.isEmpty) {
      val lastConstructorParameterIndex = constructorParameters.foldLeft(-1)(_ max _.index)
      require(lastConstructorParameterIndex + 1 == constructorParameters.size,
        "Constructor parameter list for %s is incomplete or inconsistent: found parameter with index %d, found constructor parameters%".
        format(beanDescriptor.toString, lastConstructorParameterIndex, constructorParameters.size))
    }

    require(constructorParameters.size == constructor.getParameterTypes.size,
      "Constructor parameter list for %s is incomplete: discovered %d parameters of %d. Declare missing parameters with val or var.".
      format(beanDescriptor.toString, constructorParameters.size, constructor.getParameterTypes.size))

    constructorParameters.sortWith(_.index < _.index).toList
  }

  private val unsetConstructorParams = new BitSet()
  for (i <- 0 to constructorParams.size - 1)
    unsetConstructorParams.add(i)

  val mutableProperties = properties collect {
    case mp: MutablePropertyDescriptor if !mp.isInstanceOf[ConstructorParameter] => mp
  }

  val lastMutablePropertyIndex = mutableProperties.foldLeft(-1)(_ max _.index)
  private val unsetMutableProperties = new BitSet()
  for (prop <- mutableProperties)
    unsetMutableProperties.add(prop.index)

  def newBuilder = new BeanBuilder {
    val beanDescriptor = BeanBuilderFactory.this.beanDescriptor
    def constructor = BeanBuilderFactory.this.constructor

    def constructorParams = BeanBuilderFactory.this.constructorParams
    def lastConstructorParamIndex = BeanBuilderFactory.this.constructorParams.size - 1
    protected val unsetConstructorParams = BeanBuilderFactory.this.unsetConstructorParams.clone()

    def mutableProperties = BeanBuilderFactory.this.mutableProperties
    def lastMutablePropertyIndex = BeanBuilderFactory.this.lastMutablePropertyIndex
    protected val unsetMutableProperties = BeanBuilderFactory.this.unsetMutableProperties.clone()
  }
}