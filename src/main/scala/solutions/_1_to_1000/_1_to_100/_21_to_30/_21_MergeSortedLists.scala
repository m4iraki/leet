package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

import solutions._1_to_1000._1_to_100._1_to_10._2_AddTwoNumbers.*

object _21_MergeSortedLists extends Solution[(ListNode, ListNode), ListNode] {

  def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode =
    if list1 == null then list2
    else if list2 == null then list1
    else {
      @scala.annotation.tailrec
      def inner(
        list1: ListNode,
        list2: ListNode,
        accum: ListNode,
        first: ListNode,
      ): ListNode =
        if list1 == null then {
          accum.next = list2
          first
        } else if list2 == null then {
          accum.next = list1
          first
        } else if list1.x < list2.x then {
          val next = new ListNode(list1.x, null)
          accum.next = next
          inner(list1.next, list2, next, first)
        } else {
          val next = new ListNode(list2.x, null)
          accum.next = next
          inner(list1, list2.next, next, first)
        }
      val fakeNode = new ListNode(0, null)
      inner(list1, list2, fakeNode, fakeNode).next
    }

  def run: ((ListNode, ListNode)) => ListNode = mergeTwoLists

  def samples: Seq[((ListNode, ListNode), ListNode)] = List(
    (ListNode.from(1, 2, 4), ListNode.from(1, 3, 4)) ->
      ListNode.from(1, 1, 2, 3, 4, 4),
    (null, null) -> null,
    (null, ListNode.from(0)) -> ListNode.from(0),
  )

}
