package io.github.m4iraki

@main
def main(): Unit = {
  given Eq[List[Int]] = Eq.unordered
  given Ordering[List[Int]] = Eq.unordered
  given e2: Eq[List[List[Int]]] = Eq.unordered
  given o2: Ordering[List[List[Int]]] = Eq.unordered
  Solution.measure(
    io.github.m4iraki.solutions._1_to_1000._1_to_100._11_to_20._15_ThreeSum,
    runs = 1,
    warmup = 0,
  )

}
