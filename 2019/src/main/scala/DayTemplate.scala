import zio.console.{Console, putStrLn}
import zio.{App, Task, ZIO}

object DayTemplate extends App {

  val prog: ZIO[Console, Throwable, Unit] =
    for {
      _ <- InputParser.parseFileWithCommaSeparatedLine(" ")
    } yield ()

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    prog.foldM(
      error => putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1),
      _ => ZIO.succeed(0)
    )
}
