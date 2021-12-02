import org.specs2.mutable.Specification

class OpCodeSpec extends Specification {

  "OpCode " should {
    "return the correct opcode when running getOpCode with a single digit integer" in {
      OpCode.getOpCode(1) should_== Add
    }
    "return the correct opcode when running getOpCode with a multi digit integer" in {
      OpCode.getOpCode(1101) should_== Add
    }
    "return an empty list when running getParameterModes with a single digit integer" in {
      OpCode.getParameterModes(1) should_== List.empty
    }
    "return an empty list when running getParameterModes with a double digit integer" in {
      OpCode.getParameterModes(99) should_== List.empty
    }

    "return the correct parameters when running getParameterModes with a multi digit(>2) integer" in {
      OpCode.getParameterModes(1001) should_== List(Position, Immediate)
    }
  }
}
