package io.github.m4iraki

import scala.collection.IterableOnceOps

trait Eq[A] {
  def equals(a: A, b: A): Boolean
}

object Eq {
  def fromEquals[A]: Eq[A] = _ equals _

  given Eq[Int] = fromEquals
  given Eq[Double] = fromEquals
  given Eq[String] = fromEquals
  given Eq[Boolean] = fromEquals

  import scala.math.Ordering.Implicits.given

  def unordered[F[X] <: IterableOnceOps[X, F, F[X]], A: Eq: Ordering]
    : Eq[F[A]] & Ordering[F[A]] =
    new Eq[F[A]] with Ordering[F[A]] {
      def equals(a: F[A], b: F[A]): Boolean = compare(a, b) == 0

      def compare(x: F[A], y: F[A]): Int = {
        val sortedX = x.toSeq.sorted
        val sortedY = y.toSeq.sorted
        Ordering[Seq[A]].compare(sortedX, sortedY)
      }
    }

  def orderedArray[A: Eq]: Eq[Array[A]] =
    (a: Array[A], b: Array[A]) => a.sameElements(b)

  def unorderedArray[A: Eq: Ordering]: Eq[Array[A]] & Ordering[Array[A]] =
    new Eq[Array[A]] with Ordering[Array[A]] {
      def equals(a: Array[A], b: Array[A]): Boolean = compare(a, b) == 0

      def compare(x: Array[A], y: Array[A]): Int = {
        val sortedX = x.sorted
        val sortedY = y.sorted
        Ordering[Seq[A]].compare(sortedX, sortedY)
      }

    }

  def orderedList[A: Eq: Ordering]: Eq[List[A]] =
    fromEquals[List[A]]

  def apply[A](
    using
    eq: Eq[A],
  ): Eq[A] = eq

  given Eq[Unit] = (_, _) => true
}
