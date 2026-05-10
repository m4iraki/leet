package io.github.m4iraki
package solutions._1_to_1000._1_to_100._11_to_20

object _14_LongestCommonPrefix extends Solution[Array[String], String] {

  def longestCommonPrefix(strs: Array[String]): String =
    if strs.isEmpty then ""
    else if strs.length == 1 then strs(0)
    else {
      val head = strs.head
      val tail = strs.tail
      val len = head.length
      @scala.annotation.tailrec
      def loop(idx: Int): String =
        if idx == len then head
        else if tail.forall(
            str => str.length > idx && str(idx) == head(idx),
          ) then loop(idx + 1)
        else head.substring(0, idx)

      loop(0)
    }

  def run: Array[String] => String = longestCommonPrefix

  def samples: Seq[(Array[String], String)] = List(
    Array("flower", "flow", "flight") -> "fl",
    Array("dog", "racecar", "car") -> "",
  )

}
