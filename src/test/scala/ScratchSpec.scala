import org.specs2.mutable.Specification

import scala.collection.immutable

class ScratchSpec extends Specification {
  "Scratch" should {
    "scratch" in {
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

      val indexed = groupPassportData(input, 0, Map.empty)
      val grouped = indexed.groupBy(_._2).map(_._2.keys.mkString(" "))

      val validPassportElements = List( "ecl", "pid", "eyr", "hcl", "byr","iyr", "hgt" )

      val validPassports = grouped.filter(x =>
        validPassportElements.forall(id => x.contains(s"$id:"))
      )

      print(validPassports.size)


      1 should_== 1
    }
  }

  def groupPassportData(input: List[String], currentPassportId: Int, newPassport: Map[String, Int]): Map[String, Int] = input match {
    case lastElem :: Nil => newPassport + (lastElem -> currentPassportId)
    case head :: tail =>
      if (head == "") groupPassportData(tail, currentPassportId + 1, newPassport)
      else {
        groupPassportData(tail, currentPassportId, newPassport + (head -> currentPassportId))
      }
  }



}