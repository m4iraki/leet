package io.github.m4iraki
package solutions

object _9_PalindromeNumber extends Solution[Int, Boolean] {

  def isPalindrome(x: Int): Boolean = {
    if x < 0 then return false
    if x < 10 then return true
    if x % 10 == 0 then return false
    @scala.annotation.tailrec
    def loop(f: Int, t: Int): Boolean =
      if f == t then true
      else if t > f then false
      else {
        val quo = f / 10
        (quo == t) || loop(quo, t * 10 + f % 10)
      }
    loop(x, 0)
  }

  def naive(x: Int): Boolean =
    x.toString == x.toString.reverse

  def naiveOptimized(x: Int): Boolean = (x >= 0) && (
    (x < 10) || (
      (x % 10 != 0) && {
        val str = x.toString
        str == str.reverse
      }
    )
  )

  def run: Int => Boolean = isPalindrome

  def sample: (Int, Boolean) = {
    val int = util.Random.nextInt()
    val pal = int.toString == int.toString.reverse
    int -> pal
  }

  def samples: Seq[(Int, Boolean)] = List(
    121 -> true,
    -121 -> false,
    10 -> false,
  ) ::: List.fill(10)(sample)

}
