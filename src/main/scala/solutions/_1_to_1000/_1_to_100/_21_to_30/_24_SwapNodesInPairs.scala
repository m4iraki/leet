package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

import solutions._1_to_1000._1_to_100._1_to_10._2_AddTwoNumbers.ListNode

object _24_SwapNodesInPairs extends Solution[ListNode, ListNode] {

  def swapPairs(head: ListNode): ListNode =
    if head == null || head.next == null then head
    else {
      @scala.annotation.tailrec def loop(
        current: ListNode,
        prev: ListNode,
        first: ListNode,
      ): ListNode =
        if current == null || current.next == null then first
        else {
          prev.next = current.next
          current.next = current.next.next
          prev.next.next = current
          loop(current.next, current, first)
        }
      val fake = new ListNode(0, null)
      loop(head, fake, fake).next
    }

  def run: ListNode => ListNode = swapPairs

  def samples: Seq[(ListNode, ListNode)] = List(
    (null: ListNode) -> (null: ListNode),
    ListNode.from(1) -> ListNode.from(1),
    ListNode.from(1, 2, 3) -> ListNode.from(2, 1, 3),
    ListNode.from(1, 2, 3, 4) -> ListNode.from(2, 1, 4, 3),
  )

}
