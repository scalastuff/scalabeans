package org.scalastuff.scalabeans.sig

import org.junit.Test

/**
 * For some reason these test helper classes have to be at the top level scope. The test did not fail if the classes
 * were in the scope of the test class or test function.
 */
class OuterTestClass {
  class InnerTestClass
}
class Container(val inner: OuterTestClass#InnerTestClass)

class ScalaTypeCompilerTest {
  @Test
  def testInnerClassExtractable() {
    ScalaTypeCompiler.classInfoOf[Container]
  }
}
