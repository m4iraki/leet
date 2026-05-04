package io.github.m4iraki
package solutions._1_to_1000._1_to_100._1_to_10

object _1_TwoSum extends Solution[(Array[Int], Int), Array[Int]] {
  import scala.collection.mutable.HashMap as MutableMap

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val visited: MutableMap[Int, Int] =
      new MutableMap[Int, Int](nums.length, 1.0d)
    @scala.annotation.tailrec
    def inner(idx: Int): Array[Int] =
      visited.get(target - nums(idx)) match {
        case Some(thatIdx) =>
          Array(thatIdx, idx)
        case _=>
          visited.update(nums(idx), idx)
          inner(idx + 1)
      }
    inner(0)
  }

  def run: Input => Output = twoSum.tupled

  def samples: Seq[(Input, Output)] = List(
    (Array(2, 7, 11, 15), 9) -> Array(0, 1),
    (Array(3, 2, 4), 6) -> Array(1, 2),
    (Array(3, 3), 6) -> Array(0, 1),
  )

}
