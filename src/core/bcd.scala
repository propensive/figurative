/*
    Figurative, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package figurative

export Figurative.Decimal

object Figurative:
  opaque type Decimal = Long

  object Decimal:
    def apply(i: Long): Decimal =
      var cur: Long = i
      var n: Int = 0
      var bcd: Long = 0
      
      while cur != 0 do
        bcd += ((cur%10) << n)
        n += 4
        cur /= 10

      bcd
    
    def apply(str: String): Decimal =
      var acc: Long = 0L
      var idx: Int = 0
      val len = str.length
      
      while idx < len do
        acc <<= 4
        acc |= str.charAt(idx) - 48
        idx += 1
      
      acc

  extension (left: Decimal)
    def string: String =
      var idx = 16 - (java.lang.Long.numberOfLeadingZeros(left)/4)
      var current: Long = left
      if left == 0 then "0" else
        val array = new Array[Char](idx)
        
        while idx > 0 do
          idx -= 1
          array(idx) = ((current & 15) + 48).toChar
          current >>= 4
        
        String(array)

    def long: Long =
      var total: Long = 0
      var cur: Long = left
      var mul: Long = 1
      
      while cur > 0 do
        total += (cur & 15)*mul
        mul *= 10
        cur >>= 4
      
      total

    infix def + (right: Decimal): Decimal =
      val sum = left + right + 0x0666666666666666L
      val overflow = ~(sum ^ left ^ right) & 0x1111111111111111L
      sum - ((overflow >> 2) | (overflow >> 3))
    
    infix def - (right: Decimal): Decimal =
      val diff: Long = left - right
      val underflow: Long = ~(diff ^ left ^ right) & 0x1111111111111111L
      diff - ((underflow >> 2) | (underflow >> 3))


extension (long: Long) def bin: String =
  val str = java.lang.Long.toBinaryString(long).nn
  (("0"*(64 - str.length))+str).nn.grouped(4).mkString(" ")
