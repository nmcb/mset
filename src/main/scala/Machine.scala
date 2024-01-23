object Machine extends App:

  println("the truth about (natural) numbers")
  println(s"1 + 1 == 2  evaluates to ${1 + 1 == 2}  ")
  println(s"1 + 2 == 3  evaluates to ${1 + 2 == 3}  ")
  println(s"2 + 2 == 4  evaluates to ${2 + 2 == 4}  ")
  println(s"1 + 3 == 4  evaluates to ${1 + 3 == 4}  ")
  println(s"1 + 4 == 5  evaluates to ${1 + 4 == 5}  ")
  println(s"2 + 3 == 5  evaluates to ${2 + 3 == 5}  ")
  println(s"5 + 5 == 10 evaluates to ${5 + 5 == 10} ")
  println("")

  println("the truth about (real) floating point numbers")
  println(s"1.0 + 1.0 == 2.0  evaluates to ${1.0 + 1.0 == 2.0}  ")
  println(s"1.0 + 2.0 == 3.0  evaluates to ${1.0 + 2.0 == 3.0}  ")
  println(s"2.0 + 2.0 == 4.0  evaluates to ${2.0 + 2.0 == 4.0}  ")
  println(s"1.0 + 3.0 == 4.0  evaluates to ${1.0 + 3.0 == 4.0}  ")
  println(s"1.0 + 4.0 == 5.0  evaluates to ${1.0 + 4.0 == 5.0}  ")
  println(s"2.0 + 3.0 == 5.0  evaluates to ${2.0 + 3.0 == 5.0}  ")
  println(s"5.0 + 5.0 == 10.0 evaluates to ${5.0 + 5.0 == 10.0} ")
  println("")

  println("hint - spot the falsehood")
  println(s"0.1 + 0.1 == 0.2  evaluates to ${0.1 + 0.1 == 0.2} ")
  println(s"0.1 + 0.2 == 0.3  evaluates to ${0.1 + 0.2 == 0.3} ")
  println(s"0.2 + 0.2 == 0.4  evaluates to ${0.2 + 0.2 == 0.4} ")
  println(s"0.1 + 0.3 == 0.4  evaluates to ${0.1 + 0.3 == 0.4} ")
  println(s"0.1 + 0.4 == 0.5  evaluates to ${0.1 + 0.4 == 0.5} ")
  println(s"0.2 + 0.3 == 0.5  evaluates to ${0.2 + 0.3 == 0.5} ")
  println(s"0.5 + 0.5 == 1.0  evaluates to ${0.5 + 0.5 == 1.0} ")
  println("")

  println("why does 0.1 + 0.2 == 0.3 evaluate to false ? because !")
  println(s"0.1       is in memory : '${0.1.toBinaryString}'")
  println(s"0.2       is in memory : '${0.2.toBinaryString}'")
  println(s"0.1 + 0.2 is in memory : '${(0.1 + 0.2).toBinaryString}'")
  println(s"0.3       is in memory : '${0.3.toBinaryString}'")


// util
import java.lang.{Long => JavaLong, Double => JavaDouble}
import java.math.{BigInteger => JavaBigInteger}

extension (d: Double) def toBinaryString: String =
  JavaLong.toBinaryString(JavaDouble.doubleToRawLongBits(d))

extension (s: String) def toDouble: Double =
  JavaDouble.longBitsToDouble(new JavaBigInteger(s, 2).longValue())
