package io.github.m4iraki
package solutions._1_to_1000._1_to_100._1_to_10

object _8_StringToInteger extends Solution[String, Int] {

  import scala.annotation.tailrec
  val maxInit: Int = 214748364
  val maxTailPos: Int = 7
  val maxTailNeg: Int = 8

  def myAtoi(s: String): Int = {
    val length = s.length
    if length == 0 then return 0
    @tailrec def idxUntil(idx: Int, pred: Char => Boolean): Int =
      if idx == length || !pred(s.charAt(idx)) then idx
      else idxUntil(idx + 1, pred)
    val start = idxUntil(0, _.isWhitespace)
    if length == start then return 0
    val startChar = s.charAt(start)
    val minus = startChar == '-'
    val startSign = startChar == '+' || minus
    if startSign || startChar.isDigit then {
      val leadingZeros =
        idxUntil(start + { if startSign then 1 else 0 }, _ == '0')
      val sign = if minus then -1 else 1
      val maxTail = if minus then maxTailNeg else maxTailPos
      val overflowValue = if minus then Int.MinValue else Int.MaxValue
      @tailrec def makeInt(idx: Int, acc: Int): Int =
        if idx == length then acc
        else {
          val char = s.charAt(idx)
          if !char.isDigit then acc
          else {
            val charValue = char.asDigit
            val overflow = (acc == maxInit && charValue >= maxTail) ||
              acc > maxInit
            if overflow then overflowValue
            else makeInt(idx + 1, acc * 10 + charValue)
          }
        }
      makeInt(leadingZeros, 0) * sign
    } else 0
  }

  def run: String => Int = myAtoi

  def samples: Seq[(String, Int)] = List(
    "42" -> 42,
    " -042" -> -42,
    "1337c0d3" -> 1337,
    "0-1" -> 0,
    "words and 987" -> 0,
    s"${Int.MaxValue}1" -> Int.MaxValue,
    s"${Int.MinValue}1" -> Int.MinValue,
    "" -> 0,
  )

}
