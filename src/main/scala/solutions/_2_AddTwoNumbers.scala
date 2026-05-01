package io.github.m4iraki
package solutions

object _2_AddTwoNumbers
  extends Solution[
    (_2_AddTwoNumbers.ListNode, _2_AddTwoNumbers.ListNode),
    _2_AddTwoNumbers.ListNode,
  ] {

  import scala.annotation.tailrec

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    val result = new ListNode(0, null)
    @tailrec def inner(
      l: ListNode,
      r: ListNode,
      keep: Int,
      tail: ListNode,
    ): Unit = {
      val lnull = l == null
      val rnull = r == null
      if (lnull && rnull) {
        if keep > 0 then tail.next = new ListNode(keep, null)
      } else {
        val s = {
          if lnull then 0 else l.x
        } + {
          if rnull then 0 else r.x
        } + keep
        val k = s / 10
        val n = s % 10
        val t = new ListNode(n, null)
        tail.next = t
        inner(
          { if lnull then null else l.next },
          { if rnull then null else r.next },
          k,
          t,
        )
      }
    }
    inner(l1, l2, 0, result)
    result.next
  }

  def addTwoNumbersRec(l1: ListNode, l2: ListNode): ListNode = {
    @tailrec
    def inner(
      l: ListNode,
      r: ListNode,
      acc: ListNode => ListNode,
      keep: Int,
    ): ListNode = (Option(l), Option(r)) match {
      case (None, None) =>
        if keep > 0 then acc(new ListNode(keep, null)) else acc(null)
      case (a, b) =>
        val s = a.map(_.x).getOrElse(0) + b.map(_.x).getOrElse(0) + keep
        val k = s / 10
        val n = s % 10
        inner(
          a.map(_.next).orNull,
          b.map(_.next).orNull,
          (ln => new ListNode(n, ln)).andThen(acc),
          k,
        )
    }

    inner(l1, l2, identity, 0)
  }

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object ListNode {

    def from(ints: Int*): ListNode =
      ints.foldRight(null: ListNode) {
        case (int, ln) =>
          new ListNode(int, ln)
      }

    def toList(listNode: ListNode): List[Int] = {
      @tailrec
      def inner(l: ListNode, acc: List[Int]): List[Int] =
        Option(l.next) match {
          case Some(value) => inner(value, acc.appended(l.x))
          case None        => acc.appended(l.x)
        }
      inner(listNode, Nil)
    }

    given Show[ListNode] with {

      def show(a: ListNode): String =
        toList(a).mkString("[", ", ", "]")

    }

    given Eq[ListNode] with {

      def equals(a: ListNode, b: ListNode): Boolean =
        toList(a) == toList(b)

    }

  }

  def run: Input => Output = addTwoNumbers.tupled

  def samples: Seq[(Input, Output)] = List(
    (ListNode.from(2, 4, 3), ListNode.from(5, 6, 4)) -> ListNode.from(7, 0, 8),
    (ListNode.from(0), ListNode.from(0)) -> ListNode.from(0),
    (ListNode.from(9, 9, 9, 9, 9, 9, 9), ListNode.from(9, 9, 9, 9)) ->
      ListNode.from(8, 9, 9, 9, 0, 0, 0, 1),
  )

}
