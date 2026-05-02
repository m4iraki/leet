package io.github.m4iraki

trait Show[A] {
  def show(a: A): String
}

object Show {

  def fromToString[A]: Show[A] = _.toString
  given Show[Int] = fromToString
  given Show[String] = fromToString
  given Show[Boolean] = fromToString

  given Show[Double] with {
    def show(a: Double): String = f"$a%,.2f"
  }

  given [A](
    using
    s: Show[A],
  ): Show[Array[A]] with {
    def show(a: Array[A]): String = a.map(s.show).mkString("[", ",", "]")
  }

}
