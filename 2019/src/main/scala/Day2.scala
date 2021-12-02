import zio.console._
import zio.{App, Task, ZIO}

object Day2 extends App {

  val intCodeComputer: ZIO[Console, Throwable, Unit] =
    for {
      input <- InputParser.parseFileWithCommaSeparatedLine("day2.txt")
      convertedInput <- Task(input(0).toVector.map(_.toInt))
      testProgramInput <- Task(
        initialiseProgram(convertedInput, noun = 12, verb = 2)
      )
      testProgramOutput <- runProgram(testProgramInput)
      _ <- putStrLn(s"Output of test program $testProgramOutput")
      inputResult <- runProgramToFindOutput(convertedInput, 19690720)
      _ <- putStrLn(s"Input pair to get expected output $inputResult")
    } yield ()

  def initialiseProgram(input: Vector[Int],
                        noun: Int,
                        verb: Int): Vector[Int] = {
    val splitProgram = input.splitAt(3)
    Vector(splitProgram._1(0), noun, verb) ++ splitProgram._2
  }

  def runProgram(programInput: Vector[Int]): Task[Int] = Task {
    val programResult = IntcodeComputer
      .executeInstructions(Computer(0, programInput, Vector.empty), List(0))
    programResult.instructions(0)
  }

  def runProgramToFindOutput(programInput: Vector[Int],
                             expectedOutput: Int): Task[Int] = Task {

    val matchedTuples: Seq[(Int, Int)] = for {
      noun <- 0 to 99
      verb <- 0 to 99
      newInput = initialiseProgram(programInput, noun, verb)
      output = IntcodeComputer
        .executeInstructions(Computer(0, newInput, Vector.empty), List(0))
      if output.instructions(0) == expectedOutput

    } yield (noun, verb)

    val tupleOutput = matchedTuples(0)
    (100 * tupleOutput._1) + tupleOutput._2
  }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    intCodeComputer.foldM(
      error => putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1),
      _ => ZIO.succeed(0)
    )

}
