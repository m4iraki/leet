package io.github.m4iraki
import solutions.*
@main
def main(): Unit = {
  Solution.measure(MedianOf2Arrays_4, runs = 5, warmup = 0)
}
// [1 2 3] [4 5] sum = 5 toSkip = 2
// [1 2 3] [4 5 6] sum = 6 toSkip = 2
// [1 2 3 4] [5 6 7] sum = 7 toSkip = 3