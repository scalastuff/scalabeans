package org.scalastuff.scalabeans.testmono

import org.scalatest.matchers.ShouldMatchers
import org.scalastuff.scalabeans.format.Format
import org.scalastuff.scalabeans.format.StringFormat


trait MonoBehaviours {
  type Mono[A]

  def shouldEqual[A] = new Function2[A, A, Unit] with ShouldMatchers {
    def apply(observed: A, expected: A) {
      observed should equal(expected)
//      if (observed != expected)
//        sys.error("No match: \n" + observed + "\n" + expected)
    }
  }

  def checkInverse[A <: AnyRef: Manifest](testValues: A*)(check: ((A, A) => Unit) = shouldEqual[A])

  def createMono[A <: AnyRef: Manifest]: Mono[A]
}

trait FormatBehaviours extends MonoBehaviours {

  type Mono[A] = Format[A]

  def checkInverse[A <: AnyRef: Manifest](testValues: A*)(check: ((A, A) => Unit) = shouldEqual[A]) {
    val mono = createMono[A]

    for (a <- testValues) {
      val buffer = mono.toByteArray(a)

      val bufferStr =
        mono match {
          case _: StringFormat[A] => new String(buffer)
          case _ => buffer map ("%02X" format _) mkString " "
        }

      println("%s (%s), %d bytes: %s".format(
        manifest[A].erasure.getSimpleName,
        mono.getClass.getSimpleName stripSuffix "Format$",
        buffer.length,
        bufferStr))

      val deser = mono.readFrom(buffer)

      check(a, deser)
    }
  }
}