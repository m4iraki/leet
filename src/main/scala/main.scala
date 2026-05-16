package io.github.m4iraki

@main
def main(): Unit = {
  given [T: Eq: Ordering]: Eq[List[T]] = Eq.unordered[List, T]
  Solution.measure(
    io.github.m4iraki.solutions._1_to_1000._1_to_100._11_to_20._17_LetterCombinationsOfPhoneNumber,
    runs = 1,
    warmup = 0,
  )

}
