import zio.console.{Console, putStrLn}
import zio.{App, Task, ZIO}

import scala.annotation.tailrec

object Day6 extends App {

  @tailrec
  def countDistance(orbitMap: Map[String, String],
                    orbitee: String,
                    count: Int,
                    toElement: String = "COM"): Int = {
    if (!orbitMap.contains(orbitee) || orbitee == toElement)
      count
    else {
      countDistance(orbitMap, orbitMap(orbitee), count + 1, toElement)
    }
  }

  @tailrec
  def getPathToCom(orbitMap: Map[String, String],
                   orbitee: String,
                   path: List[String]): List[String] = {
    if (!orbitMap.contains(orbitee)) path
    else {
      val nextOrbitee = orbitMap(orbitee)
      getPathToCom(orbitMap, nextOrbitee, nextOrbitee :: path)
    }
  }

  def findSplittingElement(pathYOU: List[String],
                           pathSAN: List[String]): String = {
    val intersect = pathYOU.intersect(pathSAN)
    intersect.last
  }

  val prog: ZIO[Console, Throwable, Unit] =
    for {
      input <- InputParser.readFileAsList("day6.txt")
//      input <- Task(List("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"))
//      input <- Task(List("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"))
      orbitMap <- Task(input.map { s =>
        val items = s.split(')')
        items(1) -> items(0)
      }.toMap)
      allLengths <- Task(orbitMap.map { f: (String, String) =>
        countDistance(orbitMap, f._1, 0)
      })
      sum <- Task(allLengths.sum)
      _ <- putStrLn(s"Total sum $sum")
      pathYOU <- Task(getPathToCom(orbitMap, "YOU", List.empty))
      pathSAN <- Task(getPathToCom(orbitMap, "SAN", List.empty))
      splitPoint <- Task(findSplittingElement(pathYOU, pathSAN))
      _ <- putStrLn(s"Split point $splitPoint")
      lengthYOU <- Task(countDistance(orbitMap, orbitMap("YOU"), 0, splitPoint))
      lengthSAN <- Task(countDistance(orbitMap, orbitMap("SAN"), 0, splitPoint))
      _ <- putStrLn(
        s"You $lengthYOU and me $lengthSAN together ${lengthYOU + lengthSAN}"
      )
    } yield ()

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    prog.foldM(
      error => putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1),
      _ => ZIO.succeed(0)
    )
}
