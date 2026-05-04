package io.github.m4iraki
package solutions._1_to_1000._401_to_500._481_to_490

object _485_MaxConsecutiveOnes extends Solution[Array[Int], Int] {

  def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
    val length = nums.length
    @scala.annotation.tailrec
    def loop(idx: Int, max: Int, current: Int): Int =
      if length == idx then math.max(max, current)
      else if nums(idx) == 1 then loop(idx + 1, max, current + 1)
      else {
        val newMax = math.max(max, current)
        if length - idx < newMax then newMax
        else loop(idx + 1, newMax, 0)
      }
    loop(0, 0, 0)
  }

  def run: Array[Int] => Int = findMaxConsecutiveOnes

  def samples: Seq[(Array[Int], Int)] = List(
    Array(1, 1, 0, 1, 1, 1) -> 3,
    Array(1, 0, 1, 1, 0, 1) -> 2,
  )

}
