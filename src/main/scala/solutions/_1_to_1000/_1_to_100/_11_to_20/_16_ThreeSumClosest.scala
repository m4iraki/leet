package io.github.m4iraki
package solutions._1_to_1000._1_to_100._11_to_20

object _16_ThreeSumClosest extends Solution[(Array[Int], Int), Int] {

  def threeSumClosest(nums: Array[Int], target: Int): Int =
    if nums.length <= 3 then nums.sum
    else {
      val sorted = nums.sorted
      val length = sorted.length

      @scala.annotation.tailrec
      def search(idx: Int, step: Int, limit: Int, prev: Int): Int =
        if idx == limit then limit
        else if sorted(idx + step) == prev then
          search(idx + step, step, limit, prev)
        else idx + step

      @scala.annotation.tailrec
      def loop(
        idx: Int,
        left: Int,
        right: Int,
        closest: Int,
      ): Int =
        if idx >= length - 2 then closest
        else {
          val fst = sorted(idx)
          if left >= right then {
            val next = search(idx, 1, length - 2, fst)
            loop(next, next + 1, length - 1, closest)
          } else {
            val snd = sorted(left)
            val trd = sorted(right)
            val sum = fst + snd + trd
            if sum == target then sum
            else {
              val nClosest =
                if math.abs(sum - target) < math.abs(closest - target)
                then sum
                else closest
              if sum > target then
                loop(idx, left, search(right, -1, left, trd), nClosest)
              else loop(idx, search(left, 1, right, snd), right, nClosest)
            }
          }
        }
      loop(0, 1, length - 1, sorted(0) + sorted(1) + sorted(2))
    }

  def run: ((Array[Int], Int)) => Int = threeSumClosest

  def samples: Seq[((Array[Int], Int), Int)] = List(
    (Array(-1, 2, 1, -4), 1) -> 2,
    (Array(0, 0, 0), 1) -> 0,
  )

}
