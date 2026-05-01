package io.github.m4iraki
package solutions

object _5_LongestPalindromic extends Solution[String, String] {

  import scala.annotation.tailrec

  opaque type Range = Long

  object Range {
    def apply(start: Int, len: Int): Range = (start.toLong << 32) | (len.toLong & 0xFFFFFFFFL)

    extension (r: Range)
      inline def start: Int = (r >> 32).toInt
      inline def len: Int = r.toInt
      inline def to: Int = start + len
  }
  import Range.*
  def longestPalindrome(s: String): String = {
    val length = s.length
    if length < 2 then s
    else {
      @tailrec def expand(left: Int, right: Int): Range = {
        if left < 0 || right >= length || s(left) != s(right)
        then Range(left + 1, right - left - 1)
        else expand(left - 1, right + 1)
      }

      @tailrec def idxWhile(idx: Int, char: Char): Int =
        if idx < length && s(idx) == char then idxWhile(idx + 1, char)
        else idx - 1

      @tailrec def loop(idx: Int, best: Range): Range =
        if length - idx <= best.len / 2 then best
        else {
          val consecutive = idxWhile(idx + 1, s(idx))
          val current = expand(idx, consecutive)
          if current.len > best.len then loop(consecutive + 1, current)
          else loop(consecutive + 1, best)
        }
      val best =       loop(0, Range(0, 1))
      s.substring(best.start, best.to)
    }
  }

  def naive(s: String): String = {
    val len = s.length
    val pairs = (0 until len).map {
      i => i -> stack(s, i)
    }
    val pair = pairs.maxBy {
      case (f, t) => t - f
    }
    s.substring(pair._1, pair._2)
  }

  def stack(s: String, from: Int): Int = {
    val start = s(from)
    @tailrec def inner(to: Int, stacks: Vector[Int]): Int =
      if to == from then stacks.maxOption.getOrElse(from) + 1
      else {
        val char = s(to)
        val filtered = stacks.filter {
          end =>
            val idx = from + end - to
            val c = (idx >= to) || s(idx) == char
            c
        }.appendedAll(Option.when(start == char)(to))
        inner(to - 1, filtered)
      }
    inner(s.length - 1, Vector.empty)
  }

  def run: String => String = longestPalindrome

  def samples: Seq[(String, String)] = List(
    "babad" -> "bab",
    "cbbd" -> "bb",
  )

}
