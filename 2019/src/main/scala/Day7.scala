import zio.console.{Console, putStrLn}
import zio.{App, Task, ZIO}

object Day7 extends App {

  val prog: ZIO[Console, Throwable, Unit] =
    for {
      fileInput <- InputParser.parseFileWithCommaSeparatedLine("day7.txt")
      instructions <- Task(fileInput(0).map(_.toInt).toVector)
//      instructions <- Task( Vector(3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0) ) // 54321
//      instructions <- Task(Vector(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)) //65210
//      instructions <- Task(Vector(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
//        27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5))//139629729
//      output <- Task(getHighestThrust(instructions))
//      _ <- putStrLn(s"Output: ${output._2}, phases used ${output._1.mkString(",")}")
      output2 <- Task(continuouslyRun(instructions))
      _ <- putStrLn(s"output ${output2}")
    } yield ()

  def getHighestThrust(instructions: Vector[Int]): (IndexedSeq[Int], Int) = {
    val outputs = (0 to 4).permutations.map(currentList =>
      runAmplifiers(instructions, currentList)
    )

    outputs.maxBy(_._2)
  }

  def runAmplifiers(instructions: Vector[Int], phaseSettings: IndexedSeq[Int]): (IndexedSeq[Int], Int) = {
    val initComputer = Computer(0, instructions, Vector.empty)
      val ampA = runProgram(initComputer, List(phaseSettings(0), 0))
      val ampB = runProgram(initComputer, List(phaseSettings(1), ampA.diagnostics(0)))
      val ampC = runProgram(initComputer, List(phaseSettings(2), ampB.diagnostics(0)))
      val ampD = runProgram(initComputer, List(phaseSettings(3), ampC.diagnostics(0)))
      val ampE = runProgram(initComputer, List(phaseSettings(4), ampD.diagnostics(0)))

    (phaseSettings, ampE.diagnostics(0))
  }

  def continuouslyRun(instructions: Vector[Int]): (IndexedSeq[Int], Int) = {
    val outputs = (5 to 9).permutations.map{phaseSettings =>
      val initComputer = Computer(0, instructions, Vector.empty)
      val ampA = runProgram(initComputer, List(phaseSettings(0), 0))
      val ampB = runProgram(initComputer, List(phaseSettings(1), ampA.diagnostics(0)))
      val ampC = runProgram(initComputer, List(phaseSettings(2), ampB.diagnostics(0)))
      val ampD = runProgram(initComputer, List(phaseSettings(3), ampC.diagnostics(0)))
      val ampE = runProgram(initComputer, List(phaseSettings(4), ampD.diagnostics(0)))

      (phaseSettings, feedbackLoop(instructions, ampA, ampB, ampC, ampD, ampE).diagnostics(0))
    }
    outputs.maxBy(_._2)
  }

  def feedbackLoop(instructions: Vector[Int], compA: Computer, compB: Computer, compC: Computer, compD: Computer, compE: Computer): Computer = {
    if(compE.status == Halted)
      compE
    else {
      val ampA = runProgram(compA.resetStatus, List(compE.diagnostics(0)))
      val ampB = runProgram(compB.resetStatus, List(ampA.diagnostics(0)))
      val ampC = runProgram(compC.resetStatus, List(ampB.diagnostics(0)))
      val ampD = runProgram(compD.resetStatus, List(ampC.diagnostics(0)))
      val ampE = runProgram(compE.resetStatus, List(ampD.diagnostics(0)))
      feedbackLoop(instructions, ampA, ampB, ampC, ampD, ampE)
    }
  }

  def runProgram(computer: Computer, input: List[Int]): Computer = {
    IntcodeComputer
      .executeInstructions(computer, input)
  }
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    prog.foldM(
      error => putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1),
      _ => ZIO.succeed(0)
    )
}
