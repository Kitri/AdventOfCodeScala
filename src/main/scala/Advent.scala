object Advent {

  def processReportRepair(): Unit = {
    val input = InputParser.parseInputToInt("day1.txt")
    val correctValueForReport = ExpenseReport.fixReportError(input, 2)
    val correctValueForReport2 = ExpenseReport.fixReportError(input, 3)

    println(s"The correct value for the report is $correctValueForReport and $correctValueForReport2")
  }

  def countInvalidPasswords(): Unit = {
    val input = InputParser.parseInput("day2.txt")
    val invalidPasswords = PasswordManager.countValidPasswords(input)

    println(s"Invalid password count: $invalidPasswords")

  }

  def main(args: Array[String]): Unit = {
    processReportRepair()
    countInvalidPasswords()

  }
}
