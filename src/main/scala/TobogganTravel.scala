import scala.annotation.tailrec
import scala.util.Try

object TobogganTravel {
  case class Passport( byr: String, cid: Option[String], ecl: String, eyr: String, hcl: String, hgt: String, iyr: String, pid: String )

  def countTreesTrajectory(map: List[String], rightMove: Int, downMove: Int): Int = {
    val multiplyBy = (map.length * rightMove / map.head.length) + 1
    val replicated = map.map(x => (1 to multiplyBy).map(_ => x).mkString)
    replicated.zipWithIndex.count {
      case (str, index) =>
        index != 0 && (index % downMove == 0) && str.charAt(index/downMove*rightMove) == '#'
    }
  }

  def validateDigit(len: Int, min: Int, max: Int, value: String): Boolean ={
    val intVal = value.toInt
    value.length == len && intVal >= min && intVal <= max
  }

  def validateHeight(height: String): Boolean = {
    if(height.contains("cm")) {
      val h = height.split("cm").head.toInt
      h >= 150 && h <= 193
    }
    else if(height.contains("in")) {
      val h = height.split("in").head.toInt
      h >= 59 && h <= 76

    }
    else false
  }

  def validateHair(hair: String): Boolean = {
    hair.startsWith("#") && hair.length == 7
  }

  def validateEye(eyecol: String): Boolean = {
    val valid = List("amb","blu","brn","gry","grn","hzl","oth")
    valid.contains(eyecol)
  }

  def validatePid(pid: String): Boolean = {
    val pidInt = Try(pid.toInt)
    pidInt.isSuccess && pid.length == 9
  }

  def validate(passport: Option[Passport]): Boolean = {
    if(passport.isEmpty) false
    else {
      val pass = passport.get
      validateDigit(4, 1920, 2002, pass.byr) &&
        validateDigit(4, 2010, 2020, pass.iyr) &&
        validateDigit(4, 2020, 2030, pass.eyr) &&
        validateHeight(pass.hgt) &&
        validateHair(pass.hcl) &&
        validateEye(pass.ecl) &&
        validatePid(pass.pid)
    }
  }

  def countValidPassports(input: List[String]): Int = {
    val mapped = input.map(y => getPassportElements(y))
    val validated = mapped.filter(validate)

    validated.size
  }

  private def getPassportElements(passport: String): Option[Passport] = {
    val elements = passport.split(" ") //List("byr:xxx", "cid:yyy")
    val mapped = elements.map{x =>
      val splitted = x.split(":")
      (splitted.head -> splitted(1))
    }.toMap

    val validPassportElements = List("byr", "ecl", "eyr", "hcl", "hgt", "iyr","pid")
    val cid = mapped.get("cid")

    val pass = validPassportElements.map(v => v -> mapped.get(v)).filter(_._2.isDefined).toMap

    if(pass.size == 7) {
      Some(Passport(
        byr = pass("byr").get,
        cid = cid,
        ecl = pass("ecl").get,
        eyr = pass("eyr").get,
        hcl = pass("hcl").get,
        hgt = pass("hgt").get,
        iyr = pass("iyr").get,
        pid = pass("pid").get,
      ))
    }
    else None
  }
}
