package figurative

import probably.*
import gossamer.*

object Tests extends Suite(t"Figuratively tests"):
  def run(): Unit =
    test(t"Roundtrip Long to BCD conversion"):
      Decimal(123456789L).long
    .assert(_ == 123456789L)

    test(t"Add two numbers"):
      val x = Decimal(101)
      val y = Decimal(2250)
      x.plus(y)
    .assert(_ == Decimal(2351))
