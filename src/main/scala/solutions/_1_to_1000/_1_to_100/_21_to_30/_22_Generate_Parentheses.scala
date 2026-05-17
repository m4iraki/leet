package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

object _22_Generate_Parentheses extends Solution[Int, List[String]] {

  def generateParenthesis(n: Int): List[String] = {
    def inner(opened: Int, closed: Int, current: String): List[String] = {
      if opened == n then List(current + ")" * (n - closed))
      else if opened == closed then inner(opened + 1, closed, current + "(")
      else
        inner(opened + 1, closed, current + "(") :::
          inner(opened, closed + 1, current + ")")
    }
    inner(0, 0, "")
  }

  def run: Int => List[String] = generateParenthesis

  def samples: Seq[(Int, List[String])] = List(
    3 -> List("((()))", "(()())", "(())()", "()(())", "()()()"),
    4 -> List(
      "(((())))",
      "((()()))",
      "((())())",
      "((()))()",
      "(()(()))",
      "(())(())",
      "(()()())",
      "(()())()",
      "()((()))",
      "()(()())",
      "(())()()",
      "()(())()",
      "()()(())",
      "()()()()",
    ),
    1 -> List("()"),
  )

}
