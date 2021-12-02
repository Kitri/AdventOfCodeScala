object ExpenseReport {
  def findSumOf2020(input: List[Int]): (Int, Int) = {
    input.map{ number =>
      (2020 - number -> number)
    }.filter(x => input.contains(x._1)).head
  }

  def fixReportError(input: List[Int], numberOfElements: Int): Int = {
    val combos = input.combinations(numberOfElements).toList
    combos.filter(_.sum == 2020).head.product
  }
}
