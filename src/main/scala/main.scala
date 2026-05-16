package io.github.m4iraki

@main
def main(): Unit = {
  given [T: Eq: Ordering]: Eq[List[T]] = Eq.unordered[List, T]
  given [T: Eq: Ordering]: Ordering[List[T]] = Eq.unordered[List, T]
  Solution.measure(
    io.github.m4iraki.solutions._1_to_1000._1_to_100._11_to_20._18_FourSum,
    runs = 1,
    warmup = 0,
  )

}
