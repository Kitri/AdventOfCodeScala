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

    "find valid passports" in {
      val input =List(
        "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
        "byr:1937 iyr:2017 cid:147 hgt:183cm",
        "",
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
        "hcl:#cfa07d byr:1929",
        "",
        "hcl:#ae17e1 iyr:2013",
        "eyr:2024",
        "ecl:brn pid:760753108 byr:1931",
        "hgt:179cm",
        "",
        "hcl:#cfa07d eyr:2025 pid:166559648",
        "iyr:2011 ecl:brn hgt:59in",
      )

      TobogganTravel.countValidPassports(input) should_== 2
    }

    "find seat rows and columns" in {
      "FBFBBFFRLR".splitAt(7) should_== ("FBFBBFF", "RLR")
      TobogganTravel.findPosition("FBFBBFF".toCharArray.toList, 0, 127) should_== 44
      TobogganTravel.findPosition("BFFFBBF".toCharArray.toList, 0, 127) should_== 70
      TobogganTravel.findPosition("FFFBBBF".toCharArray.toList, 0, 127) should_== 14
      TobogganTravel.findPosition("BBFFBBF".toCharArray.toList, 0, 127) should_== 102
      TobogganTravel.findPosition("RLR".toCharArray.toList, 0, 7) should_== 5
      TobogganTravel.findPosition("RRR".toCharArray.toList, 0, 7) should_== 7
      TobogganTravel.findPosition("RLL".toCharArray.toList, 0, 7) should_== 4
    }

    "find seat IDs" in {
      val input = List("BFFFBBFRRR","FFFBBBFRRR","BBFFBBFRLL")
      val expectedOutput = List(567, 119, 820)

      TobogganTravel.getFlightSeatIds(input) should_== expectedOutput
    }

    "count answers" in {
      val input = List("abc", "abc", "abac","aaaa", "b")

      input.map(_.toCharArray.distinct.length).sum should_== 11
    }
  }
}