package io.github.m4iraki
package solutions._1_to_1000._1_to_100._31_to_40

object _31_NextPermutation extends Solution[Array[Int], Unit] {

  @scala.annotation.tailrec
  def findPivot(nums: Array[Int], idx: Int): Int =
    if idx < 0 then idx
    else if nums(idx) < nums(idx + 1) then idx
    else findPivot(nums, idx - 1)

  @scala.annotation.tailrec
  def findGreater(
    nums: Array[Int],
    len: Int,
    idx: Int,
    value: Int,
    minIdx: Int = 0,
    minValue: Int = Int.MaxValue,
  ): Int =
    if idx == len then minIdx
    else {
      val num = nums(idx)
      if num > value then findGreater(nums, len, idx + 1, value, idx, num)
      else findGreater(nums, len, idx + 1, value, minIdx, minValue)
    }

  def swap(
    nums: Array[Int],
    i1: Int,
    i2: Int,
  ): Unit = {
    nums.update(i2, nums(i1) ^ nums(i2))
    nums.update(i1, nums(i1) ^ nums(i2))
    nums.update(i2, nums(i1) ^ nums(i2))
  }

  @scala.annotation.tailrec
  def reverse(
    nums: Array[Int],
    left: Int,
    right: Int,
  ): Unit =
    if left >= right then {}
    else {
      swap(nums, left, right)
      reverse(nums, left + 1, right - 1)
    }

  def nextPermutation(nums: Array[Int]): Unit = {
//    println("nextPermutation")
//    println(nums.mkString("init=[", ",", "]"))
    val len = nums.length
    if len <= 1 then {}
    else {
      val pivot = findPivot(nums, len - 2)
//      println(s"pivot = $pivot")
      if pivot == -1 then java.util.Arrays.sort(nums)
      else {
        val greater =
          findGreater(
            nums,
            len,
            pivot + 1,
            nums(pivot),
          )
        swap(nums, pivot, greater)
        java.util.Arrays.sort(nums, pivot + 1, len)
      }
//      println(nums.mkString("res =[", ",", "]"))
    }
  }

  // 1 2 3 4
  // 1 2 4 3
  // 1 3 2 4
  // 1 3 4 2
  // 1 4 2 3
  // 1 4 3 2

  // 2 1 3 4
  // 2 1 4 3
  // 2 3 1 4
  // 2 3 4 1
  // 2 4 1 3
  // 2 4 3 1

  // 3 1 2 4
  // 3 1 4 2
  // 3 2 1 4
  // 3 2 4 1
  // 3 4 1 2
  // 3 4 2 1

  // 4 1 2 3
  // 4 1 3 2
  // 4 2 1 3
  // 4 2 3 1
  // 4 3 1 2
  // 4 3 2 1
  def run: Array[Int] => Unit = nextPermutation

  def samples: Seq[(Array[Int], Unit)] = Nil
}
