package io.github.m4iraki
package solutions._1_to_1000._1_to_100._31_to_40

object _33_SearchInRotatedSortedArray extends Solution[(Array[Int], Int), Int] {

  def search(nums: Array[Int], target: Int): Int = {
    def inner(
      left: Int,
      right: Int,
    ): Int = {
      val lV = nums(left)
      val rV = nums(right)

      if lV == target then left
      else if rV == target then right
      else if left + 1 >= right then -1
      else {
        val split = left + (right - left) / 2
        val sV = nums(split)

        if sV == target then split
        else {
          val ll =
            if (sV < lV) || (lV < target && target < sV)
            then inner(left + 1, split - 1)
            else -1

          if ll >= 0 then ll
          else if (rV < sV) || (sV < target && target < rV)
          then inner(split + 1, right - 1)
          else -1
        }
      }
    }

    inner(0, nums.length - 1)
  }

  def run: ((Array[Int], Int)) => Int = search

  def sample: ((Array[Int], Int), Int) = {
    val len = util.Random.nextInt(1000) + 1
    val shift = util.Random.nextInt(len)
    val array = Array.fill(len)(util.Random.nextInt(1000) - 50).distinct.sorted
    val resultingArray =
      array.takeRight(shift)
        .appendedAll(array.take(len - shift))
    val item = util.Random.nextInt(1000) - 50
    (resultingArray, item) -> resultingArray.indexOf(item)
  }

  def samples: Seq[((Array[Int], Int), Int)] = List(
    (Array(4, 5, 6, 7, 0, 1, 2), 0) -> 4,
    (Array(4, 5, 6, 7, 0, 1, 2), 3) -> -1,
    (Array(1), 0) -> -1,
  ) ::: List.fill(15)(sample)

}
