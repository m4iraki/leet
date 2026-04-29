package io.github.m4iraki

trait Eq[A] {
  def equals(a: A, b: A): Boolean
}

object Eq {

  given [A: Eq: Ordering]: Eq[Array[A]] with {

    def equals(a: Array[A], b: Array[A]): Boolean =
      a.sorted.sameElements(b.sorted)

  }

  given Eq[Int] with {
    def equals(a: Int, b: Int): Boolean = a == b
  }
  def apply[A](using eq: Eq[A]): Eq[A] = eq
}
