package figurative

export FigurativeOpaques.Decimal

object FigurativeOpaques:
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

  extension (left: Decimal)
    def long: Long =
      var total: Long = 0
      var cur: Long = left
      var mul: Long = 1
      
      while cur > 0 do
        total += (cur & 15)*mul
        mul *= 10
        cur >>= 4
      
      total
    
    def plus(right: Decimal): Decimal =
      println("left = "+left.long)
      println("right = "+right.long)
      val sum: Long = left + right + 0x6666666666666666L
      
      println("rsum = "+(left + right))
      println("sum = "+sum)
      val carry: Long = ~(sum ^ left ^ right) & 0x1111111111111111L
      println("carry = "+(sum ^ left ^ right))
      sum - ((carry >> 2) | (carry >> 3)): Decimal


