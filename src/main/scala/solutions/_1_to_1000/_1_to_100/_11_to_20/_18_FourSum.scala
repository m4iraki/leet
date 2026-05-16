package io.github.m4iraki
package solutions._1_to_1000._1_to_100._11_to_20

object _18_FourSum extends Solution[(Array[Int], Int), List[List[Int]]] {

  def fourSum(nums: Array[Int], target: Int): List[List[Int]] = {
    val sorted = nums.sorted
    val length = sorted.length
    if length < 4 then Nil
    else {
      @scala.annotation.tailrec
      def search(idx: Int, step: Int, limit: Int, prev: Int): Int =
        if idx == limit then limit
        else if sorted(idx + step) == prev then
          search(idx + step, step, limit, prev)
        else idx + step

      @scala.annotation.tailrec
      def loop(
        i: Int,
        k: Int,
        left: Int,
        right: Int,
        acc: List[List[Int]],
      ): List[List[Int]] = {
        if i >= length - 3 then acc
        else if k >= length - 2 then {
          val next = search(i, 1, length - 3, sorted(i))
          loop(next, next + 1, next + 2, length - 1, acc)
        } else if left >= right then {
          val next = search(k, 1, length - 2, sorted(k))
          loop(i, next, next + 1, length - 1, acc)
        } else {
          val _1 = sorted(i)
          val _2 = sorted(k)
          val _3 = sorted(left)
          val _4 = sorted(right)
          val sumL = _1.toLong + _2 + _3 + _4
          if sumL > Int.MaxValue then
            loop(i, k, left, search(right, -1, left, _4), acc)
          else if sumL < Int.MinValue then
            loop(i, k, left, search(right, -1, left, _4), acc)
          else {
            val sum = sumL.toInt
            if sum == target then {
              val nextL = search(left, 1, right, _3)
              val nextR = search(right, -1, left, _4)
              loop(i, k, nextL, nextR, List(_1, _2, _3, _4) :: acc)
            } else if sum > target then
              loop(i, k, left, search(right, -1, left, _4), acc)
            else loop(i, k, search(left, 1, right, _3), right, acc)
          }
        }
      }

      loop(0, 1, 2, length - 1, Nil)
    }
  }

  def run: ((Array[Int], Int)) => List[List[Int]] = fourSum

  def samples: Seq[((Array[Int], Int), List[List[Int]])] = List(
    (Array(1, 0, -1, 0, -2, 2), 0) ->
      List(List(-2, -1, 1, 2), List(-2, 0, 0, 2), List(-1, 0, 0, 1)),
    (Array(2, 2, 2, 2, 2), 8) -> List(List(2, 2, 2, 2)),
    (Array(-3, -1, 0, 2, 4, 5), 0) -> List(List(-3, -1, 0, 4)),
    (Array(1000000000, 1000000000, 1000000000, 1000000000), -294967296) -> Nil,
  )

}
