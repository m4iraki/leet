package io.github.m4iraki
package solutions

object MedianOf2Arrays_4 extends Solution[(Array[Int], Array[Int]), Double] {

  import scala.annotation.tailrec

  def naive(nums1: Array[Int], nums2: Array[Int]): Double = {
    val l1 = nums1.length
    val l2 = nums2.length
    val sum = l1 + l2
    val toSkip = (sum - 1) / 2
    val odd = sum % 2 == 1
    @tailrec def skip(skipped1: Int, skipped2: Int): Double = {
      if skipped1 + skipped2 == toSkip then {
        if odd then {
          if skipped1 == l1 then nums2(skipped2)
          else if skipped2 == l2 then nums1(skipped1)
          else math.min(nums1(skipped1), nums2(skipped2))
        } else {
          def orMax(array: Array[Int], idx: Int): Int =
            array.applyOrElse(idx, _ => Int.MaxValue)
          List(
            orMax(nums1, skipped1),
            orMax(nums1, skipped1 + 1),
            orMax(nums2, skipped2),
            orMax(nums2, skipped2 + 1),
          ).sorted.take(2).sum / 2.0d
        }
      } else {
        if skipped1 == l1 then skip(skipped1, skipped2 + 1)
        else if skipped2 == l2 || nums1(skipped1) <= nums2(skipped2) then
          skip(skipped1 + 1, skipped2)
        else skip(skipped1, skipped2 + 1)
      }
    }
    skip(0, 0)
  }

  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    if nums1.isEmpty then single(nums2)
    else if nums2.isEmpty then single(nums1)
    else if nums1.length + nums2.length == 2 then (nums1(0) + nums2(0)) / 2.0d
    else {
      val smol = if nums1.length > nums2.length then nums2 else nums1
      val s = smol.length
      val big = if nums1.length <= nums2.length then nums2 else nums1
      val b = big.length
      val sum = s + b
      val half = (sum + 1) / 2
      val odd = sum % 2 == 1
      @tailrec def split(l: Int, r: Int): Double = {
        val i = (r + l) / 2
        val bI = half - i
        val sl = if i == 0 then Int.MinValue else smol(i - 1)
        val bl = if bI == 0 then Int.MinValue else big(bI - 1)
        val sr = if i == s then Int.MaxValue else smol(i)
        val br = if bI == b then Int.MaxValue else big(bI)
        if sl <= br && bl <= sr then {
          val lowMax = math.max(sl, bl)
          if odd then lowMax
          else (lowMax + math.min(sr, br)) / 2.0d
        } else if sl > br
        then split(l, i - 1)
        else split(i + 1, r)
      }
      split(0, s)
    }
  }

  def single(nums: Array[Int]): Double = {
    val mid = nums.length / 2
    if nums.length % 2 == 0 then (nums(mid) + nums(mid - 1)) / 2.0d
    else nums(mid)
  }

  def run: Input => Output = findMedianSortedArrays.tupled

  def sample(n: Int): (Input, Output) = {
    val array = Array.fill[Int](n)(util.Random.nextInt(100))
    val sorted = array.sorted
    val median = single(sorted)
    val splitAt = util.Random.between(0, n)
    val (linput, rinput) = array.splitAt(splitAt)
    (linput.sorted, rinput.sorted) -> median
  }

  def samples: Seq[(Input, Output)] = List(
    (Array(1, 3), Array(2)) -> 2.0d,
    (Array(1, 2), Array(3, 4)) -> 2.5d,
    sample(10),
    sample(50),
    sample(100),
    sample(500),
    sample(2000),
  )

}
