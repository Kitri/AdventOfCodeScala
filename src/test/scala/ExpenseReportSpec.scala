import org.specs2.mutable.Specification

class ExpenseReportSpec extends Specification {
  "Expense Report" should {
    "Find two numbers that sum to 2020" in {
      val input = List(1721, 979, 366, 299, 675, 1456)
      ExpenseReport.findSumOf2020(input) should_== (299, 1721)
    }

    "Fix error in expense report" in {
      val input = List(1721, 979, 366, 299, 675, 1456)
      val fixed = ExpenseReport.fixReportError(input, 2)
      fixed should_== 514579
    }

    "Fix error in expense report with 3 numbers" in {
      val input = List(1721, 979, 366, 299, 675, 1456)
      val fixed = ExpenseReport.fixReportError(input, 3)
      fixed should_== 241861950
    }
  }
}