package io.github.m4iraki
package solutions._1_to_1000._1_to_100._11_to_20

object _11_ContainerWithMostWater extends Solution[Array[Int], Int] {
  import scala.annotation.tailrec

  def maxArea(height: Array[Int]): Int = {
    val len = height.length
    @tailrec def loop(l: Int, r: Int, max: Int): Int =
      if l >= r then max
      else {
        val minH = math.min(height(l), height(r))
        val area = (r - l) * minH
        val mmax = math.max(area, max)
        @tailrec def next(i: Int, step: Int, cap: Int): Int =
          if i == cap then cap
          else if height(i) > minH then i
          else next(i + step, step, cap)
        val nl = next(l, 1, len - 1)
        val nr = next(r, -1, 0)
        loop(nl, nr, mmax)
      }
    loop(0, len - 1, 0)
  }

  def run: Array[Int] => Int = maxArea

  def samples: Seq[(Array[Int], Int)] = List(
    Array(1, 8, 6, 2, 5, 4, 8, 3, 7) -> 49,
    Array(1, 8, 6, 2, 100, 100, 8, 3, 7) -> 100,
    Array(1, 1) -> 1,
  )

}
