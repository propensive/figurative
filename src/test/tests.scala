package figurative

import probably.*
import gossamer.*

object Tests extends Suite(t"Figuratively tests"):
  def run(): Unit =
    for i <- 1 to 1000 do
      test(t"Roundtrip Long to BCD conversion"):
        Decimal(i).long
      .assert(_ == i)

    test(t"Add two numbers"):
      val x = Decimal(101)
      val y = Decimal(2250)
      x + y
    .assert(_ == Decimal(2351))
    
    test(t"Subtract two numbers"):
      val x = Decimal(101)
      val y = Decimal(2250)
      y - x
    .assert(_ == Decimal(2149))

    test(t"Parse a decimal number"):
      Decimal("31415926").long
    .assert(_ == 31415926)
    
    test(t"Serialize a decimal number"):
      Decimal("31415926").string
    .assert(_ == "31415926")

    val ints = new Array[Long](100000000)
    val bcdInts = new Array[Decimal](100000000)
    val stringInts = new Array[String](100000000)

    val addition = 123456789L
    val additionBcd = Decimal(123456789L)

    test(t"Set up integers array"):
      for i <- 0 until 50000000 do ints(i) = i.toLong
    .assert()

    test(t"Set up string integers array"):
      for i <- 0 until 25000000 do stringInts(i) = i.toString
    .assert()

    test(t"Set up BCD integers array"):
      for i <- 0 until 25000000 do bcdInts(i) = Decimal(i)
    .assert()
    
    test(t"Add to integers array"):
      for i <- 0 until 25000000 do ints(i) = i.toLong + addition
    .assert()

    test(t"Add to BCD integers array"):
      for i <- 0 until 25000000 do bcdInts(i) = Decimal(i) + additionBcd
    .assert()
  
    test(t"Parse BCD strings"):
      for i <- 0 until 25000000 do Decimal(stringInts(i))
    .assert()
    
    test(t"Parse and convert BCD strings"):
      for i <- 0 until 25000000 do Decimal(stringInts(i)).long
    .assert()
    
    test(t"Parse and convert strings to doubles"):
      for i <- 0 until 25000000 do stringInts(i).toDouble
    .assert()
    
    test(t"Parse int strings"):
      for i <- 0 until 25000000 do Integer.parseInt(stringInts(i))
    .assert()
    
    var str: String = ""
    test(t"Serialize integers"):
      for i <- 0 until 25000000 do str = ints(i).toString
    .assert()
    
    test(t"Serialize BCD integers"):
      for i <- 0 until 25000000 do str = bcdInts(i).string
    .assert()
    
  
    

