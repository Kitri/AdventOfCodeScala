import zio.console.{Console, putStrLn}
import zio.{App, Task, ZIO}

object Day5 extends App {

  val prog: ZIO[Console, Throwable, Unit] =
    for {
      fileInput <- InputParser.parseFileWithCommaSeparatedLine("day5.txt")
      instructions <- Task(fileInput(0).map(_.toInt).toVector)
//      instructions <- Task(
//        Vector(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20,
//          31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
//          999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99)
//      )//999 below 8, 1000 = 8, 1001 > 8
      computer <- runProgram(instructions, 5)
      _ <- putStrLn(
        s"output ${computer.instructions(0)} , diagnostics ${computer.diagnostics}"
      )
    } yield ()

  def runProgram(instructions: Vector[Int], input: Int): Task[Computer] = Task {
    IntcodeComputer
      .executeInstructions(Computer(0, instructions, Vector.empty), List(input))
  }
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    prog.foldM(
      error => putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1),
      _ => ZIO.succeed(0)
    )
}
