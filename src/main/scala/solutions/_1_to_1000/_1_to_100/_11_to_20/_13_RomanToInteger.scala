package io.github.m4iraki
package solutions._1_to_1000._1_to_100._11_to_20

object _13_RomanToInteger extends Solution[String, Int] {

  def map(char: Char): Int = char match {
    case 'I' => 1
    case 'V' => 5
    case 'X' => 10
    case 'L' => 50
    case 'C' => 100
    case 'D' => 500
    case 'M' => 1000
    case _   => 0
  }

  def romanToInt(s: String): Int = {
    @scala.annotation.tailrec
    def foldRight(idx: Int, acc: Int, max: Int): Int =
      if idx < 0 then acc
      else {
        val char = s(idx)
        val value = map(char)
        if value >= max then foldRight(idx - 1, acc + value, value)
        else foldRight(idx - 1, acc - value, value)
      }
    foldRight(s.length - 1, 0, 0)
  }

  def run: String => Int = romanToInt

  def samples: Seq[(String, Int)] = List(
    "III" -> 3,
    "LVIII" -> 58,
    "MCMXCIV" -> 1994,
  )

}
