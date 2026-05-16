package io.github.m4iraki
package solutions._1_to_1000._1_to_100._11_to_20

import solutions._1_to_1000._1_to_100._1_to_10._2_AddTwoNumbers.ListNode

object _19_RemoveNthNodeFromEnd extends Solution[(ListNode, Int), ListNode] {

  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    @scala.annotation.tailrec
    def fold(
      ln: ListNode,
      acc: List[Int],
    ): List[Int] =
      if ln == null then acc
      else fold(ln.next, ln.x :: acc)
    @scala.annotation.tailrec
    def unfold(
      list: List[Int],
      count: Int,
      acc: ListNode,
    ): ListNode =
      list match {
        case head :: tail =>
          if count == n then unfold(tail, count + 1, acc)
          else unfold(tail, count + 1, new ListNode(head, acc))
        case Nil => acc
      }
    unfold(fold(head, Nil), 1, null)
  }

  def run: ((ListNode, Int)) => ListNode = removeNthFromEnd

  def samples: Seq[((ListNode, Int), ListNode)] = List(
    (ListNode.from(1, 2, 3, 4, 5), 2) -> ListNode.from(1, 2, 3, 5),
    (ListNode.from(1), 1) -> null,
    (ListNode.from(1, 2), 1) -> ListNode.from(1),
  )

}
