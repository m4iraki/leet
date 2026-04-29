package io.github.m4iraki

trait Solution[I, O] {
  type Input = I
  type Output = O
  def run: I => O
  def samples: Seq[(I, O)]
}

object Solution {

  def measure[I, O](
    solution: Solution[I, O],
    runs: Int = 1,
    warmup: Int = 0,
  )(
    using
    eq: Eq[O],
    sh: Show[O],
  ): Unit = {
    val runtime = Runtime.getRuntime

    def runOne(io: (I, O)): String = {
      runtime.gc()
      Thread.sleep(50)
      val before = runtime.totalMemory() - runtime.freeMemory()
      val start = System.currentTimeMillis()
      val res = solution.run(io._1)
      val end = System.currentTimeMillis()
      val after = runtime.totalMemory() - runtime.freeMemory()
      val elapsed = end - start
      val taken = (after - before) / 1024
      val ok = eq.equals(res, io._2)
      val summary = f"elapsed $elapsed%,d ms. used $taken%,d KB"
      if ok then s"succeed with result ${sh.show(res)}. " + summary
      else
        s"failed with result ${sh.show(res)}. " +
          s"expected ${sh.show(io._2)}. " +
          summary
    }
    (0 until warmup).foreach {
      _ => solution.samples.foreach(runOne)
    }
    (0 until runs).foreach {
      _ => println(solution.samples.map(runOne).mkString("\n"))
    }
  }

}
