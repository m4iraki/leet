package io.github.m4iraki
package solutions._1_to_1000._1_to_100._21_to_30

import solutions._1_to_1000._1_to_100._1_to_10._2_AddTwoNumbers.*

object _23_MergeKSortedLists extends Solution[Array[ListNode], ListNode] {

  def mergeKLists(lists: Array[ListNode]): ListNode = {
    def inner(fromInclusive: Int, toExclusive: Int): ListNode = {
      val length = toExclusive - fromInclusive
      length match {
        case 0 => null: ListNode
        case 1 => lists(fromInclusive)
        case 2 => _21_MergeSortedLists.mergeTwoLists(
            lists(fromInclusive),
            lists(fromInclusive + 1),
          )
        case e =>
          val mid = length / 2
          _21_MergeSortedLists.mergeTwoLists(
            inner(fromInclusive, fromInclusive + mid),
            inner(fromInclusive + mid, toExclusive),
          )
      }
    }
    inner(0, lists.length)
  }

  def naive(lists: Array[ListNode]): ListNode =
    lists.foldLeft(null: ListNode)(_21_MergeSortedLists.mergeTwoLists)

  def run: Array[ListNode] => ListNode = mergeKLists

  def samples: Seq[(Array[ListNode], ListNode)] = List(
    Array(
      ListNode.from(1, 4, 5),
      ListNode.from(1, 3, 4),
      ListNode.from(2, 6),
    ) -> ListNode.from(1, 1, 2, 3, 4, 4, 5, 6),
    Array.empty[ListNode] -> null,
    Array(null: ListNode) -> null,
  )

}
