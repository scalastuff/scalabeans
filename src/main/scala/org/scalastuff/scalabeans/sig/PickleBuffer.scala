/**
 * Copyright (c) 2011 ScalaStuff.org (joint venture of Alexander Dvorkovyy and Ruud Diterwich)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package org.scalastuff.scalabeans.sig

import scala.reflect.generic.{ PickleBuffer => PoorPickleBuffer }
import java.nio.charset.Charset
import scala.reflect.generic.Flags

class PickleBuffer(bytes: Array[Byte]) {
  final val UTF8 = Charset.forName("UTF-8")
  
  case class SymbolInfo(nameRef: Int, ownerRef: Int, flags: Long, privateWithinRef: Option[Int], infoRef: Int) {
    override def toString = {
      privateWithinRef match {
        case None => "SymbolInfo(nameRef=%d, ownerRef=%d, flags=%#x, infoRef=%d)".format(nameRef, ownerRef, flags, infoRef)
        case Some(privateWithin) => "SymbolInfo(nameRef=%d, ownerRef=%d, flags=%#x, privateWithinRef=%d, infoRef=%d)".format(nameRef, ownerRef, flags, privateWithin, infoRef)
      }
    }
  }

  var readIndex = 0
  
  /** Read a byte */
  def readByte(): Int = {
    val x = bytes(readIndex); readIndex += 1; x
  }

  /** Read a natural number in big endian format, base 128.
   *  All but the last digits have bit 0x80 set.*/
  def readNat(): Int = readLongNat().toInt

  def readLongNat(): Long = {
    var b = 0L
    var x = 0L
    do {
      b = readByte()
      x = (x << 7) + (b & 0x7f)
    } while ((b & 0x80) != 0L);
    x
  }

  /** Read a long number in signed big endian format, base 256. */
  def readLong(len: Int): Long = {
    var x = 0L
    var i = 0
    while (i < len) {
      x = (x << 8) + (readByte() & 0xff)
      i += 1
    }
    val leading = 64 - (len << 3)
    x << leading >> leading
  }
  
  def readString(len: Int): String = {
    val str = new String(bytes, readIndex, len, UTF8)
    readIndex += len
    str
  }
  
  def readSymbolInfo(len: Int) = {
    val start = readIndex
    val end = readIndex + len
    val nameRef = readNat()
    val ownerRef = readNat()
    val flags = Flags.pickledToRawFlags(readLongNat())
    val x = readNat()
    
    val symbolInfo = 
	    if (readIndex == end) SymbolInfo(nameRef, ownerRef, flags, None, x)
	    else SymbolInfo(nameRef, ownerRef, flags, Some(x), readNat())
	    
//    if (readIndex != end)
//      error("at: " + readIndex + " expected:" + end)
      
    symbolInfo
  }
  
  def skip(count: Int) {
	  readIndex += count
  }
  
  def createIndex: Array[Int] = {
    val index = new Array[Int](readNat()) // nbEntries_Nat
    for (i <- 0 until index.length) {
      index(i) = readIndex
      readByte() // skip type_Nat
      readIndex = readNat() + readIndex // read length_Nat, jump to next entry
    }
    index
  }
}