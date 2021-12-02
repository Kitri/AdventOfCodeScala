import scala.annotation.tailrec
import zio.{App, Task, ZIO}
import zio.console._

object Day1 extends App {

  val massCalculator: ZIO[Console, Throwable, Unit] =
    for {
      input <- InputParser.readFileAsList("day1.txt")
      massOfModules <- Task(input.map(_.toInt))
      sumOfFuel <- Task(massOfModules.map(calculateFuelForMass).sum)
      _ <- putStrLn(s"Simple Fuel calculation $sumOfFuel")
      fuelForAllModules <- Task(
        massOfModules.map(calculateFuelForModule(_, 0)).sum
      )
      _ <- putStrLn(s"Total fuel $fuelForAllModules")
    } yield ()

  def calculateFuelForMass(mass: Int): Int = (mass / 3) - 2

  //e.g.
  //(14/3) - 2 = 2
  //(72/3) - 2 = 22; (22/3) - 2 = 5; (5/3) - 2 = -1; Thus total = 22+5 = 27
  // Total = 29
  @tailrec
  def calculateFuelForModule(mass: Int, totalFuel: Int): Int = {
    val fuel = calculateFuelForMass(mass)

    if (fuel <= 0) {
      totalFuel
    } else {
      calculateFuelForModule(fuel, totalFuel + fuel)
    }
  }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    massCalculator.fold(_ => 1, _ => 0)

}
