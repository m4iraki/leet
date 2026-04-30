package io.github.m4iraki

trait Eq[A] {
  def equals(a: A, b: A): Boolean
}

object Eq {

  def unorderedArray[A: Eq: Ordering]: Eq[Array[A]] =
    (a: Array[A], b: Array[A]) => a.sorted.sameElements(b.sorted)

  def orderedArray[A: Eq: Ordering]: Eq[Array[A]] =
    (a: Array[A], b: Array[A]) => a.sameElements(b)

  given [A: Eq: Ordering]: Eq[Array[A]] = unorderedArray
  given Eq[Int] with {
    def equals(a: Int, b: Int): Boolean = a == b
  }
  given Eq[Double] with {
    def equals(a: Double, b: Double): Boolean = a == b
  }

  def apply[A](
    using
    eq: Eq[A],
  ): Eq[A] = eq

}
