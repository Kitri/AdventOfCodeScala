import org.specs2.mutable.Specification

class TobogganTravelSpec extends Specification {
  "Toboggan Travel" should {
    "Find trees in trajectory" in {
      val input =List(
        "..##.......", //0
        "#...#...#..", //1
        ".#....#..#.", //2 [1]
        "..#.#...#.#", //3
        ".#...##..#.", //4 [2]
        "..#.##.....", //5
        ".#.#.#....#", //6 [3]
        ".#........#", //7
        "#.##...#...", //8 [4]
        "#...##....#", //9
        ".#..#...#.#") //10 [5]

      val count1 = TobogganTravel.countTreesTrajectory(input, 1, 1)
      count1 should_== 2

      val count3 = TobogganTravel.countTreesTrajectory(input, 3, 1)
      count3 should_== 7

      val count5 = TobogganTravel.countTreesTrajectory(input, 5, 1)
      count5 should_== 3

      val count7 = TobogganTravel.countTreesTrajectory(input, 7, 1)
      count7 should_== 4

      val count1_2 = TobogganTravel.countTreesTrajectory(input, 1, 2)
      count1_2 should_== 2

    }
  }
}