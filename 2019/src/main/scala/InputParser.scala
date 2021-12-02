import zio.Task

import scala.io.Source

object InputParser {

  def readFileAsList(filename: String): Task[List[String]] = Task {

    val bufferedSource = Source.fromFile(filename)

    val lines = bufferedSource.getLines().toList

    bufferedSource.close()

    lines
  }

  def parseFileWithCommaSeparatedLine(
    filename: String
  ): Task[List[List[String]]] = {
    for {
      lines <- readFileAsList(filename)
      list <- Task(lines.map(line => line.split(",").toList))
    } yield list
  }
}
