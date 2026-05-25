package io.github.m4iraki
package solutions._1_to_1000._1_to_100._31_to_40

object _32_LongestValidParentheses extends Solution[String, Int] {

  def longestValidParentheses(s: String): Int = {
    val len = s.length

    @scala.annotation.tailrec
    def loop(
      idx: Int,
      max: Int,
      stack: List[Int],
      failedAt: Int,
    ): Int =
      if idx == len then max
      else if s(idx) == '('
      then
        loop(
          idx = idx + 1,
          max = max,
          stack = idx :: stack,
          failedAt = failedAt,
        )
      else
        stack match {
          case Nil =>
            loop(idx = idx + 1, max = max, stack = Nil, failedAt = idx)
          case opened :: next =>
            loop(
              idx = idx + 1,
              max = math.max(max, idx - next.headOption.getOrElse(failedAt)),
              stack = next,
              failedAt = failedAt,
            )
        }

    loop(idx = 0, max = 0, stack = Nil, failedAt = -1)
  }

  def run: String => Int = longestValidParentheses

  def samples: Seq[(String, Int)] = List(
    "(()" -> 2,
    ")()())" -> 4,
    "()(()" -> 2,
    "(()()" -> 4,
    "" -> 0,
    ")(" -> 0,
    ")(((((()())()()))()(()))(" -> 22,
  )

}
