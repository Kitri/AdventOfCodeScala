import org.specs2.mutable.Specification

class EncoderSpec extends Specification {
  "Scratch" should {
    "scratch" in {
      val input = List[BigInt](
        35 ,20 ,15 ,25 ,47 ,40 ,62 ,55 ,65 ,95 ,102 ,117 ,150 ,182 ,127 ,219 ,299 ,277 ,309 ,576
      )
      val output = Encoder.findXMASMistake(input, 5)
      output should_== 127
    }
    "xx" in {
      Encoder.checkSum(List(1,2,3,4,5), 7) should_== true
      Encoder.checkSum(List(1,2,3,4,5), 5) should_== true
      Encoder.checkSum(List(1,2,3,4,5), 10) should_== false
      Encoder.checkSum(List(1,2,3,4,5), 11) should_== false
    }
    "find sum in list" in {
      val input = List[BigInt](
        35 ,20 ,15 ,25 ,47 ,40 ,62 ,55 ,65 ,95 ,102 ,117 ,150 ,182 ,127 ,219 ,299 ,277 ,309 ,576
      )
      val output = Encoder.findSumInList(input, 127, 0, List.empty)
      output should_== List[BigInt](15,25,47,40)
      output.max + output.min should_== 62
    }
  }
}