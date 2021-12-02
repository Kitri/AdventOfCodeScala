import Grid.GridPosition
import zio.console.{Console, putStrLn}
import zio.{App, Task, ZIO}

import scala.annotation.tailrec

/*
wire1 <- Task(
  List("R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72")
)
wire2 <- Task(
  List("U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83")
)
      wire1 <- Task( List( "R98", "U47", "R26", "D63", "R33", "U87", "L62", "D20", "R33", "U53", "R51" ) )
      wire2 <- Task( List("U98", "R91", "D20", "R16", "D67", "R40", "U7", "R15", "U6", "R7") ) */
object Day3 extends App {

  val wireTracer: ZIO[Console, Throwable, Unit] =
    for {
      input <- InputParser.parseFileWithCommaSeparatedLine("day3.txt")
      wire1 = input(0)
      wire2 = input(1)
      pathWire1 <- traceWire(wire1)
      pathWire2 <- traceWire(wire2)
      intersection <- Task(findIntersectingPositions(pathWire1, pathWire2))
      shortestPath <- Task(intersection.map(_.getManhattanDistance).min)
      leastSteps <- Task(intersection.map(_.getCombinedSteps).min)
      _ <- putStrLn(
        s"Shortest Path: ${shortestPath.toString}; Least steps: ${leastSteps.toString}"
      )

    } yield ()

  private def findIntersectingPositions(
    firstWire: List[Grid.GridPosition],
    secondWire: List[Grid.GridPosition]
  ): List[WireIntersection] = {
    val intersection = firstWire.intersect(secondWire)
    intersection.map(
      i =>
        WireIntersection(i, firstWire.indexOf(i) + 1, secondWire.indexOf(i) + 1)
    )
  }

  def traceWire(instructions: List[String]) = Task {
    applyInstructions(instructions, GridPosition(0, 0), List.empty)
  }

  @tailrec
  private def applyInstructions(instructions: List[String],
                                currentPosition: GridPosition,
                                path: List[GridPosition]): List[GridPosition] =
    instructions match {
      case Nil => path
      case instruction :: tail =>
        val instructionPath: List[GridPosition] =
          Grid.determinePath(instruction, currentPosition)

        applyInstructions(tail, instructionPath.last, path ++ instructionPath)
    }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    wireTracer.foldM(
      error => putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1),
      _ => ZIO.succeed(0)
    )
}

case class WireIntersection(position: GridPosition,
                            nrOfStepsFirstWire: Int,
                            nrOfStepsSecondWire: Int) {
  def getManhattanDistance: Int = {
    position.row.abs + position.column.abs
  }
  def getCombinedSteps: Int = {
    nrOfStepsFirstWire + nrOfStepsSecondWire
  }
}
