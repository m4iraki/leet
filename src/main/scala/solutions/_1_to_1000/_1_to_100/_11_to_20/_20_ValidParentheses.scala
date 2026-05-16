package io.github.m4iraki
package solutions._1_to_1000._1_to_100._11_to_20

object _20_ValidParentheses extends Solution[String, Boolean] {

  def isValid(s: String): Boolean = {
    val len = s.length
    def pair(c: Char): Char =
      c match {
        case '(' => ')'
        case '{' => '}'
        case '[' => ']'
      }
    @scala.annotation.tailrec
    def lookup(idx: Int, stack: List[Char]): Boolean =
      if idx == len then stack.isEmpty
      else
        s(idx) match {
          case c if c == '(' || c == '{' || c == '[' =>
            lookup(idx + 1, c :: stack)
          case c =>
            stack match {
              case head :: tail if c == pair(head) => lookup(idx + 1, tail)
              case _                               => false
            }
        }
    lookup(0, Nil)    
  }

  def run: String => Boolean = isValid

  def samples: Seq[(String, Boolean)] = List(
    "()" -> true,
    "()[]{}" -> true,
    "(]" -> false,
    "([])" -> true,
    "([)]" -> false,
  )

}
