package io.github.m4iraki

@main
def main(): Unit = {
  given Eq[List[Int]] = Eq.unordered
  Solution.measure(
    io.github.m4iraki.solutions._1_to_1000._1_to_100._31_to_40._32_LongestValidParentheses,
    runs = 1,
    warmup = 0,
  )
}
