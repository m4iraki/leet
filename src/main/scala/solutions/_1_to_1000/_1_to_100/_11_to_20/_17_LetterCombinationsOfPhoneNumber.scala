package io.github.m4iraki
package solutions._1_to_1000._1_to_100._11_to_20

object _17_LetterCombinationsOfPhoneNumber
  extends Solution[String, List[String]] {

  def letterCombinations(digits: String): List[String] =
    digits.foldLeft(List.empty[String]) {
      case (strings, char) =>
        val variants = char match {
          case '2' => List('a', 'b', 'c')
          case '3' => List('d', 'e', 'f')
          case '4' => List('g', 'h', 'i')
          case '5' => List('j', 'k', 'l')
          case '6' => List('m', 'n', 'o')
          case '7' => List('p', 'q', 'r', 's')
          case '8' => List('t', 'u', 'v')
          case '9' => List('w', 'x', 'y', 'z')
        }
        if strings.isEmpty then variants.map(_.toString)
        else
          for {
            string <- strings
            variant <- variants
          } yield string + variant
    }

  def run: String => List[String] = letterCombinations

  def samples: Seq[(String, List[String])] = List(
    "23" -> List("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"),
    "2" -> List("a", "b", "c"),
  )

}
