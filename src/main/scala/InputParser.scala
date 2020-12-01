import scala.io.Source

object InputParser {

  def parseInput(fileName: String): List[String] = {
      val bufferedSource = Source.fromFile(fileName)
      val lines = bufferedSource.getLines().toList
      bufferedSource.close()
      lines
  }

  def parseInputToInt(fileName: String): List[Int] = {
    parseInput(fileName).map(_.toInt )
  }

}
