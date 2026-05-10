package io.github.m4iraki
package solutions._1_to_1000._1_to_100._11_to_20

object _15_ThreeSum extends Solution[Array[Int], List[List[Int]]] {

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val sorted = nums.sorted
    val len = sorted.length
    @scala.annotation.tailrec
    def search(idx: Int, step: Int, limit: Int, prev: Int): Int =
      if idx == limit then limit
      else if sorted(idx + step) == prev then
        search(idx + step, step, limit, prev)
      else idx + step

    @scala.annotation.tailrec
    def loop(
      idx: Int,
      l: Int,
      r: Int,
      acc: List[List[Int]],
    ): List[List[Int]] =
      if idx >= len - 2 then acc
      else if l >= r then {
        val next = search(idx, 1, len - 2, sorted(idx))
        loop(next, next + 1, len - 1, acc)
      } else {
        val sum = sorted(idx) + sorted(l) + sorted(r)
        if sum == 0 then {
          val nextL = search(l, 1, r, sorted(l))
          val nextR = search(r, -1, l, sorted(r))
          loop(idx, nextL, nextR, List(idx, l, r) :: acc)
        } else if sum > 0 then loop(idx, l, search(r, -1, l, sorted(r)), acc)
        else loop(idx, search(l, 1, r, sorted(l)), r, acc)
      }
    loop(0, 1, len - 1, Nil).map(_.map(sorted.apply)).distinct
  }

  import scala.collection.mutable.HashMap as MutableMap

  def twoSum(nums: Array[Int], offset: Int, target: Int): List[List[Int]] = {
    val len = nums.length
    val visited: MutableMap[Int, Int] =
      new MutableMap[Int, Int](len - offset, 1.0d)
    @scala.annotation.tailrec
    def loop(idx: Int, acc: List[List[Int]]): List[List[Int]] =
      if idx >= len then acc
      else {
        val num = nums(idx)
        visited.get(target - num) match {
          case Some(thatIdx) =>
            loop(idx + 1, List(idx, thatIdx) :: acc)
          case _ =>
            visited.update(num, idx)
            loop(idx + 1, acc)
        }
      }
    loop(offset, Nil)
  }

  def threeSumWithTwoSum(nums: Array[Int]): List[List[Int]] = {
    val sorted = nums.sorted
    val len = sorted.length
    @scala.annotation.tailrec
    def loop(
      idx: Int,
      prev: Option[Int],
      acc: List[List[Int]],
    ): List[List[Int]] =
      if idx + 2 >= len || sorted(idx) > 0 then acc
      else {
        val num = sorted(idx)
        if prev.contains(num) then loop(idx + 1, prev, acc)
        else {
          val pairs = twoSum(sorted, idx + 1, -num)
          val res = pairs.distinct.map(idx :: _)
          loop(idx + 1, Some(num), res ::: acc)
        }
      }
    loop(0, None, Nil).map(_.map(sorted.apply)).distinct
  }

  def run: Array[Int] => List[List[Int]] = threeSum

  def samples: Seq[(Array[Int], List[List[Int]])] = List(
    Array(-1, 0, 1, 2, -1, -4) -> List(List(-1, 0, 1), List(-1, -1, 2)),
    Array(0, 1, 1) -> List(),
    Array(0, 0, 0) -> List(List(0, 0, 0)),
    new Array[Int](3000) -> List(List(0, 0, 0)),
  )

}
