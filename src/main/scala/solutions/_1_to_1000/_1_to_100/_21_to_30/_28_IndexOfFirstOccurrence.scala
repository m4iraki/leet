package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

object _28_IndexOfFirstOccurrence extends Solution[(String, String), Int] {

  def strStr(haystack: String, needle: String): Int = {
    val needleHead = needle.head
    val hlen = haystack.length
    val nlen = needle.length
    @scala.annotation.tailrec
    def lookup(
      hIdx: Int,
      nIdx: Int,
      fallbacks: List[Int],
    ): Int =
      if nIdx == nlen then hIdx - nIdx
      else if nIdx == 0 && hIdx + nlen > hlen then
        fallbacks match {
          case head :: next =>
            lookup(head + 1, 1, next)
          case Nil => -1
        }
      else {
        val hChar = haystack(hIdx)
        val nChar = needle(nIdx)
        val nFb =
          if nIdx != 0 &&
            hIdx + nlen <= hlen &&
            hChar == needleHead &&
            !fallbacks.contains(hIdx)
          then hIdx :: fallbacks
          else fallbacks
        if hChar == nChar then lookup(hIdx + 1, nIdx + 1, nFb)
        else
          nFb match {
            case head :: next =>
              lookup(head + 1, 1, next)
            case Nil => lookup(hIdx + 1, 0, Nil)
          }
      }
    lookup(0, 0, Nil)
  }

  def run: ((String, String)) => Int = strStr

  def samples: Seq[((String, String), Int)] = List(
    ("sadbutsad", "sad") -> 0,
    ("hesadbutsad", "sad") -> 2,
    ("mississippi", "issippi") -> 4,
    ("mississippi", "sippia") -> -1,
    ("leetcode", "leeto") -> -1,
  )

}
