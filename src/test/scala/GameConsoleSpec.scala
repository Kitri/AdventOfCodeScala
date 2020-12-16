import org.specs2.mutable.Specification

class GameConsoleSpec extends Specification {
  val input = List(
    "nop +0", //0
    "acc +1", //1
    "jmp +4", //2
    "acc +3", //3
    "jmp -3", //4
    "acc -99", //5
    "acc +1", //6
    "jmp -4", //7
    "acc +6" //8
  )

  "Scratch" should {
    "scratch" in {
      val output = GameConsole.getAccValue(input)
      output should_== 5
    }
  }
}