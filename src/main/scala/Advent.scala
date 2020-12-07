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

  def countTreesInTobogganTrajectory(): Unit = {
    val input = InputParser.parseInput("day3.txt")
    val trees1Right = TobogganTravel.countTreesTrajectory(input, 1, 1)
    val trees3Right = TobogganTravel.countTreesTrajectory(input, 3, 1 )
    val trees5Right = TobogganTravel.countTreesTrajectory(input, 5, 1)
    val trees7Right = TobogganTravel.countTreesTrajectory(input, 7, 1)
    val trees1Right2Down = TobogganTravel.countTreesTrajectory(input, 1, 2)

    val multiplied = BigInt(trees1Right) * BigInt(trees3Right) * BigInt(trees5Right) * BigInt(trees7Right) * BigInt(trees1Right2Down)

    println(s"Number of trees: 1 - $trees1Right; 3 - $trees3Right; 5 - $trees5Right; 7 - $trees7Right; 1/2 - $trees1Right2Down")
    print(s"Multiplied: $multiplied")
  }

  def countValidPassports(): Unit = {
    val input = InputParser.parseInput("day4.txt")
    val string = input.mkString("\n")
    val split = string.split("\n\n").map(_.replace("\n"," ")).toList

    val validPassports = TobogganTravel.countValidPassports(split)

    print(s"Valid Passports: ${validPassports}")

  }

  def main(args: Array[String]): Unit = {
//    processReportRepair()
//    countInvalidPasswords()
//    countTreesInTobogganTrajectory()
      countValidPassports()

  }
}
