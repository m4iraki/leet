package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

object _29_DivideTwoIntegers extends Solution[(Int, Int), Int] {

  def divide(dividend: Int, divisor: Int): Int =
    if dividend == Int.MinValue && divisor == -1 then Int.MaxValue
    else {
      val isNegative = (dividend < 0) ^ (divisor < 0)
      val negDividend = if dividend > 0 then -dividend else dividend
      val negDivisor = if divisor > 0 then -divisor else divisor

      @scala.annotation.tailrec
      def loop(
        dividend: Int,
        quot: Int,
      ): Int =
        if dividend > negDivisor then quot
        else {
          @scala.annotation.tailrec
          def findMaxQuot(subDiv: Int, subQout: Int): (Int, Int) =
            if subDiv >= -0x40000000 && (subDiv << 1) >= dividend then
              findMaxQuot(subDiv << 1, subQout << 1)
            else
              (subDiv, subQout)

          val (maxDivisor, maxQuot) = findMaxQuot(negDivisor, 1)
          loop(dividend - maxDivisor, quot + maxQuot)
        }

      val finalResult = loop(negDividend, 0)
      if isNegative then -finalResult else finalResult
    }

  def run: ((Int, Int)) => Int = divide

  def samples: Seq[((Int, Int), Int)] = List(
    (10, 3) -> 3,
    (1, 1) -> 1,
    (0, 1) -> 0,
    (Int.MinValue, -1) -> Int.MaxValue,
    (Int.MinValue, -3) -> -2147483648 / -3,
    (7, -3) -> -2,
  )

}
