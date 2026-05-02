package io.github.m4iraki

trait Eq[A] {
  def equals(a: A, b: A): Boolean
}

object Eq {
  def fromEquals[A]: Eq[A] = _ equals _

  given Eq[Int] = fromEquals
  given Eq[Double] = fromEquals
  given Eq[String] = fromEquals
  given Eq[Boolean] = fromEquals

  def unorderedArray[A: Eq: Ordering]: Eq[Array[A]] =
    (a: Array[A], b: Array[A]) => a.sorted.sameElements(b.sorted)

  def orderedArray[A: Eq: Ordering]: Eq[Array[A]] =
    (a: Array[A], b: Array[A]) => a.sameElements(b)

  given [A: Eq: Ordering]: Eq[Array[A]] = unorderedArray

  def apply[A](
    using
    eq: Eq[A],
  ): Eq[A] = eq

}
