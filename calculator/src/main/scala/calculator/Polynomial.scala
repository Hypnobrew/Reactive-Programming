package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    //Δ = b² - 4ac
    val first = b() * b()
    val second = 4 * a() * c()
    Signal(first - second)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / (2a)
    if (delta() < 0) Signal(Set.empty)
    else if(a() == 0) Signal(Set(Double.NaN))
    else {
      val first = (-b() + Math.sqrt(delta()) / (2 * a()))
      val second = (-b() - Math.sqrt(delta()) / (2 * a()))

      if(first != second) Signal(Set(first, second))
      else Signal(Set(first))
    }
  }
}
