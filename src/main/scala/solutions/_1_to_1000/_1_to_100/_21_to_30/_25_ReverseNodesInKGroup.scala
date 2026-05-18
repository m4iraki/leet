package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

import solutions._1_to_1000._1_to_100._1_to_10._2_AddTwoNumbers.ListNode

object _25_ReverseNodesInKGroup extends Solution[(ListNode, Int), ListNode] {

  @scala.annotation.tailrec
  def reverseK(
    k: Int,
    from: ListNode,
    to: ListNode = null,
  ): (ListNode, ListNode) = {
    if k == 0 then (to, from)
    else if from == null then (reverse(to), null)
    else {
      val next = from.next
      from.next = to
      reverseK(k - 1, next, from)
    }
  }

  @scala.annotation.tailrec
  def reverse(
    ln: ListNode,
    to: ListNode = null,
  ): ListNode =
    if ln == null then to
    else {
      val next = ln.next
      ln.next = to
      reverse(next, ln)
    }

  def reverseKGroup(head: ListNode, k: Int): ListNode = {
    @scala.annotation.tailrec
    def inner(
      ln: ListNode,
      prev: ListNode,
      fst: ListNode,
    ): ListNode =
      if ln == null then fst
      else {
        val nPrev = ln
        val (reversed, next) = reverseK(k, ln, null)
        prev.next = reversed
        inner(next, nPrev, fst)
      }
    val fake = new ListNode(0, null)
    inner(head, fake, fake).next
  }

  def run: ((ListNode, Int)) => ListNode = reverseKGroup

  def samples: Seq[((ListNode, Int), ListNode)] = List(
    (ListNode.from(1, 2, 3, 4, 5), 2) -> ListNode.from(2, 1, 4, 3, 5),
    (ListNode.from(1, 2, 3, 4, 5), 3) -> ListNode.from(3, 2, 1, 4, 5),
  )

}
