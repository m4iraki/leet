package io.github.m4iraki
package solutions._1_to_1000._1_to_100._31_to_40

object _35_SearchInsertPosition extends Solution[(Array[Int], Int), Int] {

  def searchInsert(nums: Array[Int], target: Int): Int =
    binary(nums, 0, nums.length - 1, target)

  @scala.annotation.tailrec
  def binary(
    nums: Array[Int],
    left: Int,
    right: Int,
    target: Int,
  ): Int =
    if nums(left) >= target then left
    else {
      val r = nums(right)
      if target == r then right
      else if target > r then right + 1
      else {
        val mid = left + (right - left) / 2
        val m = nums(mid)
        if target == m then mid
        else if target > m then binary(nums, mid + 1, right - 1, target)
        else binary(nums, left + 1, mid - 1, target)
      }
    }

  def run: ((Array[Int], Int)) => Int = searchInsert

  def samples: Seq[((Array[Int], Int), Int)] = List(
    (Array(1, 3, 5, 6), 5) -> 2,
    (Array(1, 3, 5, 6), 2) -> 1,
    (Array(1, 3, 5, 6), 7) -> 4,
  )

}
