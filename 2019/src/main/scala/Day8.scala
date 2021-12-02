import zio.console.{Console, putStrLn}
import zio.{App, Task, ZIO}

object Day8 extends App {

  def createImageLayers(input: List[Int], width: Int, height: Int): Seq[List[Int]] = {
   input.sliding(width * height, width * height).toList
  }

  def mergeImageLayers(imageLayers: Seq[List[Int]]): List[Int] = {
    imageLayers.foldLeft(imageLayers(0)){(x,y) => {
      for {
        (top, second) <- x zip y
      } yield if(top == 2) second else top
    }}
  }

  def printImage(mergedLayers: List[Int], width: Int): List[String] = {
    val x = mergedLayers.map(i => if(i == 0) " " else "x")
    x.sliding(width, width).map(_.mkString("")).toList
  }

  val prog: ZIO[Console, Throwable, Unit] =
    for {
      input <- InputParser.readFileAsList("day8.txt")
      pixels <- Task(input(0).toList.map(_.asDigit))
//      pixels <- Task(List(1,2,3,4,5,6,7,8,9,0,1,2))
//      pixels <- Task(List(0,2,2,2,1,1,2,2,2,2,1,2,0,0,0,0))
      imageLayers <- Task(createImageLayers(pixels, 25, 6))
//      imageLayers <- Task(createImageLayers(pixels, 2,2))
      counts <- Task(imageLayers.map(layer => Map(0 -> layer.count(_ == 0), 1 -> layer.count(_ == 1), 2 -> layer.count(_ == 2))))
      leastZeros <- Task(counts.minBy(f => f.get(0)))
      _ <- putStrLn(s"Output $leastZeros")
      finalCalc <- Task(leastZeros.getOrElse(1, 0) * leastZeros.getOrElse(2, 0))
      _ <- putStrLn(s"Answer part 1: $finalCalc")
      mergedLayers <- Task(mergeImageLayers(imageLayers))
      _ <- putStrLn(printImage(mergedLayers, 25).mkString("\n"))
    } yield ()

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    prog.foldM(
      error => putStrLn(s"Execution failed with: $error") *> ZIO.succeed(1),
      _ => ZIO.succeed(0)
    )
}
