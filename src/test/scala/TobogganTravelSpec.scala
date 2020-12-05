import org.specs2.mutable.Specification

class TobogganTravelSpec extends Specification {
  "Toboggan Travel" should {
    "Find trees in trajectory" in {
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

      val count = TobogganTravel.countTreesTrajectory(input)
      count should_== 7

    }
  }
}