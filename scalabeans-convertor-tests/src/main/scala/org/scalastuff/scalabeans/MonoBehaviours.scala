package org.scalastuff.scalabeans
import org.scalastuff.util.Format
import org.scalatest.matchers.ShouldMatchers

trait MonoBehaviours {
  type Mono[A]

  def checkInverse[A <: AnyRef : Manifest](sd: Mono[A], testValues: Iterable[A])

  def createMono[A <: AnyRef : Manifest]: Mono[A]
}

trait FormatBehaviours extends MonoBehaviours with ShouldMatchers {
  
  type Mono[A] = Format[A]
  
  def checkInverse[A <: AnyRef : Manifest](mono: Mono[A], testValues: Iterable[A]) {
    for(a <- testValues) {
	  val buffer = mono.toByteArray(a)
	  
	val bufferStr = buffer map ("%02X" format _) mkString " "

    println("%s (%s), %d bytes: %s".format(
      manifest[A].erasure.getSimpleName,
      mono.getClass.getSimpleName stripSuffix "Format$",
      buffer.length,
      bufferStr))
      
      val deser = mono.readFrom(buffer)
      
      a should equal (deser) 
    }
  }
}