import org.specs2.mutable.Specification

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable

class ScratchSpec extends Specification {
  "Scratch" should {
    "scratch" in {
      val input =List(
        "..##.......",
        "#...#...#..",
        ".#....#..#.",
        "..#.#...#.#",
        ".#...##..#.",
        "..#.##.....",
        ".#.#.#....#",
        ".#........#",
        "#.##...#...",
        "#...##....#",
        ".#..#...#.#")

      val y = input.map{x =>
        multiplyBy(x, 5)
      }
      print(y.head)
      1 should_== 1
    }
  }


  def multiplyBy(input: String, num: Int): String = {
    (1 to num).map(_ => input).mkString
  }

}