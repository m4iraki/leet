package io.github.m4iraki

@main
def main(): Unit = {
//  import solutions._1_to_1000._1_to_100._31_to_40._37_SudokuSolver
//  _37_SudokuSolver.run(_37_SudokuSolver.samples(1)._1)
  given Eq[Array[Int]] = Eq.unorderedArray
  Solution.measure(
    solutions._1_to_1000._1_to_100._31_to_40._36_SudokuValidation,
    runs = 1,
    warmup = 0,
  )
}
