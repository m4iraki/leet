package io.github.m4iraki
package solutions._1_to_10

object _7_ReverseInteger extends Solution[Int, Int] {

  import scala.annotation.tailrec

  val maxInit: Int = 214748364
  val maxTail: Int = 7
  val minInit: Int = -maxInit
  val minTail: Int = -8

  def reverse(x: Int): Int =
    if x / 10 == 0 then x
    else {
      @tailrec def loop(input: Int, output: Int, count: Int): Int = {
        val mod = input % 10
        val quo = input / 10
        if quo == 0 then {
          if count == 10 then {
            val overflow =
              (output > maxInit) ||
                (output == maxInit && mod > maxTail) ||
                (output < minInit) ||
                (output == minInit && mod < minTail)
            if overflow then 0 else output * 10 + mod
          } else output * 10 + mod
        } else loop(quo, output * 10 + mod, count + 1)
      }
      loop(x, 0, 1)
    }

  def run: Int => Int = reverse

  def sample: (Int, Int) = {
    val input = util.Random.nextInt()
    val inputStr = input.toString
    val reverseStr = inputStr.drop(if input < 0 then 1 else 0).reverse
    val ulong = reverseStr.toLong
    val long = ulong * input.sign
    val output =
      if long < Int.MinValue || long > Int.MaxValue
      then 0
      else long.toInt
    println(input -> output)
    input -> output
  }

  def samples: Seq[(Int, Int)] = List(
    120 -> 21,
    123 -> 321,
    -123 -> -321,
    sample,
    sample,
    sample,
  )

}
