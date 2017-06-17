package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val d = computeDelta(a,b,c)
    Signal(if (a() < 0) Set() else if (a() == 0) { Set( -b() / (2 * a()) ) } else Set( (-b() + d())/ (2 * a()) , (-b() - d())/ (2 * a())))
  }
}
