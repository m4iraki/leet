package io.github.m4iraki
package solutions._1_to_1000._1_to_100._31_to_40

object _36_SudokuValidation extends Solution[Array[Array[Char]], Boolean] {

  def ins(
    map: Map[Int, Set[Char]],
    idx: Int,
    char: Char,
  ): Map[Int, Set[Char]] =
    map.updatedWith(idx) {
      case Some(value) => Some(value + char)
      case None        => Some(Set(char))
    }

  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    @scala.annotation.tailrec
    def loop(
      row: Int,
      col: Int,
      rows: Map[Int, Set[Char]],
      cols: Map[Int, Set[Char]],
      doms: Map[Int, Set[Char]],
    ): Boolean =
      if row == 9 then true
      else if col == 9 then loop(row + 1, 0, rows, cols, doms)
      else {
        val char = board(row)(col)
        if char.isDigit then {
          val dom = row / 3 * 3 + col / 3
          val rowOk = rows.get(row).forall(!_(char))
          val colOk = cols.get(col).forall(!_(char))
          val domOk = doms.get(dom).forall(!_(char))
          val correct = rowOk && colOk && domOk
          correct && loop(
            row,
            col + 1,
            ins(rows, row, char),
            ins(cols, col, char),
            ins(doms, dom, char),
          )
        } else loop(row, col + 1, rows, cols, doms)
      }

    loop(0, 0, Map.empty, Map.empty, Map.empty)
  }

  def run: Array[Array[Char]] => Boolean = isValidSudoku

  def samples: Seq[(Array[Array[Char]], Boolean)] = List(
    Array(
      Array('5', '3', '.', /**/ '.', '7', '.', /**/ '.', '.', '.'),
      Array('6', '.', '.', /**/ '1', '9', '5', /**/ '.', '.', '.'),
      Array('.', '9', '8', /**/ '.', '.', '.', /**/ '.', '6', '.'),
      /*---------------------------------------------------------*/
      Array('8', '.', '.', /**/ '.', '6', '.', /**/ '.', '.', '3'),
      Array('4', '.', '.', /**/ '8', '.', '3', /**/ '.', '.', '1'),
      Array('7', '.', '.', /**/ '.', '2', '.', /**/ '.', '.', '6'),
      /*---------------------------------------------------------*/
      Array('.', '6', '.', /**/ '.', '.', '.', /**/ '2', '8', '.'),
      Array('.', '.', '.', /**/ '4', '1', '9', /**/ '.', '.', '5'),
      Array('.', '.', '.', /**/ '.', '8', '.', /**/ '.', '7', '9'),
    ) -> true,
    Array(
      Array('8', '3', '.', /**/ '.', '7', '.', /**/ '.', '.', '.'),
      Array('6', '.', '.', /**/ '1', '9', '5', /**/ '.', '.', '.'),
      Array('.', '9', '8', /**/ '.', '.', '.', /**/ '.', '6', '.'),
      /*---------------------------------------------------------*/
      Array('8', '.', '.', /**/ '.', '6', '.', /**/ '.', '.', '3'),
      Array('4', '.', '.', /**/ '8', '.', '3', /**/ '.', '.', '1'),
      Array('7', '.', '.', /**/ '.', '2', '.', /**/ '.', '.', '6'),
      /*---------------------------------------------------------*/
      Array('.', '6', '.', /**/ '.', '.', '.', /**/ '2', '8', '.'),
      Array('.', '.', '.', /**/ '4', '1', '9', /**/ '.', '.', '5'),
      Array('.', '.', '.', /**/ '.', '8', '.', /**/ '.', '7', '9'),
    ) -> false,
  )

}
