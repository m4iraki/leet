package io.github.m4iraki
package solutions._1_to_1000._1_to_100._31_to_40

object _34_FirstAndLastPosOfElement
  extends Solution[(Array[Int], Int), Array[Int]] {

  def searchRange(nums: Array[Int], target: Int): Array[Int] = {
    val someAppearance = binary(nums, 0, nums.length - 1, target)
    if someAppearance == -1 then Array(-1, -1)
    else
      Array(
        binaryLeft(nums, 0, someAppearance, target),
        binaryRight(nums, someAppearance, nums.length - 1, target),
      )
  }

  @scala.annotation.tailrec
  def binary(
    nums: Array[Int],
    left: Int,
    right: Int,
    target: Int,
  ): Int =
    if left > right then -1
    else {
      val mid = left + (right - left) / 2
      val midV = nums(mid)
      if midV == target then mid
      else if midV < target then binary(nums, mid + 1, right, target)
      else binary(nums, left, mid - 1, target)
    }

  @scala.annotation.tailrec
  def binaryLeft(
    nums: Array[Int],
    left: Int,
    right: Int,
    target: Int,
  ): Int =
    if left > right then left
    else {
      val mid = (left + right) / 2
      val midV = nums(mid)
      if midV == target then binaryLeft(nums, left, mid - 1, target)
      else binaryLeft(nums, mid + 1, right, target)
    }

  @scala.annotation.tailrec
  def binaryRight(
    nums: Array[Int],
    left: Int,
    right: Int,
    target: Int,
  ): Int =
    if left > right then right
    else {
      val mid = (left + right) / 2
      val midV = nums(mid)
      if midV == target then binaryRight(nums, mid + 1, right, target)
      else binaryRight(nums, left, mid - 1, target)
    }

  def run: ((Array[Int], Int)) => Array[Int] = searchRange

  def samples: Seq[((Array[Int], Int), Array[Int])] = List(
    (Array(5, 7, 7, 8, 8, 10), 8) -> Array(3, 4),
    (Array(5, 7, 7, 8, 8, 10), 6) -> Array(-1, -1),
    (Array.empty[Int], 0) -> Array(-1, -1),
  )

}
