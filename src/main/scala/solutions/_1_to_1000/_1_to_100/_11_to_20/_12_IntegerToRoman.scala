package io.github.m4iraki
package solutions._1_to_1000._1_to_100._11_to_20

object _12_IntegerToRoman extends Solution[Int, String] {

  val digits: List[(Int, Char, Char, Char)] = List(
    (1000, 'M', '?', '?'),
    (100, 'C', 'D', 'M'),
    (10, 'X', 'L', 'C'),
    (1, 'I', 'V', 'X'),
  )

  def digit2Roman(digit: Int, one: String, five: String, ten: String): String =
    digit match {
      case 0              => ""
      case lt4 if lt4 < 4 => one * lt4
      case 4              => one + five
      case lt9 if lt9 < 9 => five + one * (lt9 - 5)
      case 9              => one + ten
      case _              => ""
    }

  def intToRoman(num: Int): String =
    digit2Roman(num / 1000 % 10, "M", "", "") +
      digit2Roman(num / 100 % 10, "C", "D", "M") +
      digit2Roman(num / 10 % 10, "X", "L", "C") +
      digit2Roman(num % 10, "I", "V", "X")

  def run: Int => String = intToRoman

  def samples: Seq[(Int, String)] = List(
    3749 -> "MMMDCCXLIX",
    58 -> "LVIII",
    1994 -> "MCMXCIV",
  )

}
