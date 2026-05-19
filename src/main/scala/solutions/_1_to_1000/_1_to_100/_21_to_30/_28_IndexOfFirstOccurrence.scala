package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

object _28_IndexOfFirstOccurrence extends Solution[(String, String), Int] {

  def strStr(haystack: String, needle: String): Int = {

    val needleHead = needle.head
    val hlen = haystack.length
    val nlen = needle.length
    val cutoff = hlen - nlen

    @scala.annotation.tailrec
    def check(
      hayIdx: Int,
      ndlIdx: Int,
    ): Boolean = (ndlIdx == nlen) ||
      (haystack(hayIdx) == needle(ndlIdx) && check(hayIdx + 1, ndlIdx + 1))

    @scala.annotation.tailrec
    def loop(
      idx: Int,
    ): Int =
      if idx > cutoff then -1
      else if haystack(idx) == needleHead && check(idx + 1, 1) then idx
      else loop(idx + 1)

    loop(0)
  }

  def run: ((String, String)) => Int = strStr

  def samples: Seq[((String, String), Int)] = List(
    ("sadbutsad", "sad") -> 0,
    ("hesadbutsad", "sad") -> 2,
    ("mississippi", "issippi") -> 4,
    ("mississippi", "sippia") -> -1,
    ("leetcode", "leeto") -> -1,
    ("h" * 1_000_000 + "e", "h" * 500_000 + "e") -> 500_000,
  )

}
