package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

object _26_RemoveDuplicatesFromSorted extends Solution[Array[Int], Int] {

  def removeDuplicates(nums: Array[Int]): Int = {
    val length = nums.length
    @scala.annotation.tailrec
    def loop(
      idx: Int,
      count: Int,
      prev: Option[Int],
    ): Int =
      if idx == length then count
      else {
        val num = nums(idx)
        if prev.contains(num) then loop(idx + 1, count, prev)
        else {
          nums.update(count, num)
          loop(idx + 1, count + 1, Some(num))
        }
      }
    loop(0, 0, None)
  }

  def run: Array[Int] => Int = removeDuplicates

  def samples: Seq[(Array[Int], Int)] = List(
    Array(1, 1, 2) -> 2,
    Array(0, 0, 1, 1, 1, 2, 2, 3, 3, 4) -> 5,
  )

}
