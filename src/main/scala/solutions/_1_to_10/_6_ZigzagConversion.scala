package io.github.m4iraki
package solutions._1_to_10

object _6_ZigzagConversion extends Solution[(String, Int), String] {
  import scala.annotation.tailrec

  extension (b: Boolean) {
    inline def toInt: Int = if b then 1 else 0
  }

  def convert(s: String, numRows: Int): String = {
    val length = s.length
    if length <= numRows || numRows == 1 then s
    else {
      val chars = new Array[Char](length)
      val rowsOffsets = {
        val segmentLength = numRows * 2 - 2
        val segmentsCount = (length + segmentLength - 1) / segmentLength
        val partialSegmentLength = length % segmentLength
        val deficit = segmentLength - partialSegmentLength
        val result = new Array[Int](numRows)
        @tailrec def fill(row: Int, sum: Int): Array[Int] = {
          result(row - 1) = sum
          if row == numRows then {
            result
          } else {
            val count =
              if row == 1 then segmentsCount
              else {
                val modifier =
                  if partialSegmentLength == 0 then 2
                  else if partialSegmentLength < row then 0
                  else if row - 2 >= deficit then 2
                  else 1
                segmentsCount * 2 - 2 + modifier
              }
            fill(row + 1, count + sum)
          }
        }

        fill(1, 0)
      }
      @tailrec def loop(
        idx: Int,
        row: Int,
        dir: Int,
      ): String =
        if idx == length then String.valueOf(chars)
        else {
          val resultIdx = rowsOffsets(row)
          rowsOffsets(row) += 1
          chars(resultIdx) = s(idx)
          val newDir =
            if row == numRows - 1 then -1
            else if row == 0 then 1
            else dir
          loop(idx + 1, row + newDir, newDir)
        }
      loop(
        idx = 0,
        row = 0,
        dir = 1,
      )
    }
  }

  def naive(s: String, numRows: Int): String = {
    val length = s.length
    if length <= numRows || numRows == 1 then s
    else {
      val vectors = Vector.from(0 until numRows).map {
        _ => Vector.newBuilder[Char]
      }

      @tailrec def loop(idx: Int, vectorIdx: Int, change: Int): String =
        if idx == length
        then vectors.map(_.mapResult(_.mkString).result()).mkString
        else {
          vectors(vectorIdx).addOne(s(idx))
          if vectorIdx == 0 then loop(idx + 1, 1, 1)
          else if vectorIdx == numRows - 1 then loop(idx + 1, vectorIdx - 1, -1)
          else loop(idx + 1, vectorIdx + change, change)
        }

      loop(0, 0, 1)
    }
  }

  def run: Input => Output = convert.tupled

  def samples: Seq[(Input, Output)] = List(
    ("PAYPALISHIRING", 3) -> "PAHNAPLSIIGYIR",
    ("PAYPALISHIRING", 4) -> "PINALSIGYAHRPI",
    ("PAYPALISHIRING", 9) -> "PAYPGANLIIRSIH",
    ("ABCDE", 4) -> "ABCED",
    ("A", 1) -> "A",
  )

}
