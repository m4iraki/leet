package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

object _27_RemoveElement extends Solution[(Array[Int], Int), Int] {
  def removeElement(nums: Array[Int], `val`: Int): Int = removeElem(nums, `val`)

  def removeElem(nums: Array[Int], value: Int): Int = {
    val len = nums.length
    @scala.annotation.tailrec
    def loop(
      index: Int,
      count: Int,
    ): Int =
      if index == len then count
      else {
        val num = nums(index)
        if num == value then loop(index + 1, count)
        else {
          if count != index then nums.update(count, num)
          loop(index + 1, count + 1)
        }
      }
    loop(0, 0)
  }

  def run: ((Array[Int], Int)) => Int = removeElement

  def samples: Seq[((Array[Int], Int), Int)] = List(
    (Array(3, 2, 2, 3), 3) -> 2,
    (Array(0, 1, 2, 2, 3, 0, 4, 2), 2) -> 5,
  )

}
