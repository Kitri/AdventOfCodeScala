import zio.console.{Console, putStrLn}
import zio.{App, Task, ZIO}

object Day4 extends App {

  val passwordCracker: ZIO[Console, Throwable, Unit] =
    for {
      input <- Task(136818 to 685979)
      validPasswordsCount <- Task(
        input.count(password => isValidPassword(password))
      )
      _ <- putStrLn(
        s"Number of valid passwords ${validPasswordsCount.toString}"
      )
    } yield ()

  def isValidPassword(password: Int): Boolean = {
    val stringPassword = password.toString
    val chars = stringPassword.toList

    val pairStates = chars
      .sliding(2)
      .map(
        c =>
          if (c(0) == c(1)) IsMatchingAdjacent
          else if (c(1) > c(0)) IsIncreasing
          else IsDecreasing
      )
      .toList

    val repeatingDigitsCount =
      chars.groupBy(identity).view.mapValues(_.size).toMap

    applyRules(pairStates, repeatingDigitsCount, stringPassword)

  }

  private def applyRules(pairStates: List[PairState],
                         repeatingDigitsCount: Map[Char, Int],
                         password: String): Boolean = {

    pairStates.contains(IsMatchingAdjacent) &&
    !pairStates.contains(IsDecreasing) &&
    password.length == 6 &&
    repeatingDigitsCount.values.toList.contains(2)

  }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    passwordCracker.foldM(
      error => putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1),
      _ => ZIO.succeed(0)
    )
}

sealed trait PairState
case object IsIncreasing extends PairState
case object IsMatchingAdjacent extends PairState
case object IsDecreasing extends PairState
