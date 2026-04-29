package io.github.m4iraki

trait Show[A] {
  def show(a: A): String
}

object Show {

  given Show[Int] with {
    def show(a: Int): String = a.toString
  }

  given [A](
    using
    s: Show[A],
  ): Show[Array[A]] with {
    def show(a: Array[A]): String = a.map(s.show).mkString("[", ",", "]")
  }

}
