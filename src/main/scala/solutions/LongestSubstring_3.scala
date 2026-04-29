package io.github.m4iraki
package solutions

object LongestSubstring_3 extends Solution[String, Int] {

  import scala.annotation.tailrec

  def lengthOfLongestSubstring(s: String): Int = {
    val length = s.length
    if length < 2 then length
    else {
      val lastSeen = new Array[Int](128)

      @tailrec def inner(
        pos: Int,
        from: Int,
        longest: Int,
      ): Int =
        if pos >= length then math.max(pos - from, longest)
        else {
          val char = s.charAt(pos)
          val last = lastSeen(char)
          lastSeen.update(char, pos + 1)
          if last > from
          then inner(pos + 1, last, math.max(pos - from, longest))
          else inner(pos + 1, from, longest)
        }

      inner(0, 0, 0)
    }
  }

  def run: String => Int = lengthOfLongestSubstring

  def samples: Seq[(String, Int)] = List(
    "abcabcbb" -> 3,
    "bbbbb" -> 1,
    "pwwkew" -> 3,
    "aab" -> 2,
    "dvdf" -> 3,
    "aabaab!bb" -> 3,
    "au" -> 2,
  )

}
