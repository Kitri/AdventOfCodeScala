import org.specs2.mutable.Specification

class IntcodeComputerSpec extends Specification {

  "getParameterValue" should {
    "default to position value" in {

      val instructionPointer = 0
      val instructions = Vector(1, 2, 3, 4)
      IntcodeComputer.getParameterValue(
        instructionPointer + 1,
        instructions,
        0,
        List.empty
      ) should_== 3

      IntcodeComputer.getParameterValue(
        instructionPointer + 1,
        instructions,
        0,
        List(Position, Position)
      ) should_== 3

      IntcodeComputer.getParameterValue(
        instructionPointer + 1,
        instructions,
        1,
        List(Position)
      ) should_== 4
    }

    "return the value as is if the mode is immediate" in {
      val instructionPointer = 0
      val instructions = Vector(1, 2, 3, 4)
      IntcodeComputer.getParameterValue(
        instructionPointer + 1,
        instructions,
        0,
        List(Immediate, Position)
      ) should_== 2

      IntcodeComputer.getParameterValue(
        instructionPointer + 1,
        instructions,
        1,
        List(Immediate, Immediate)
      ) should_== 3
    }
  }

  "getParameters" should {
    "return 2 values from positions last value as is" in {
      val instructions = Vector(1, 2, 3, 4)
      val parameters = IntcodeComputer.getParameters(instructions, 0, 3)
      parameters should_== List(3, 4, 4)
    }

    "return 2 immediate values and last value as is" in {
      val instructions = Vector(1101, 2, 3, 4)
      val parameters = IntcodeComputer.getParameters(instructions, 0, 3)
      parameters should_== List(2, 3, 4)
    }

    "return 2 immediate value" in {
      val instructions = Vector(1105, 2, 3)
      val parameters = IntcodeComputer.getParameters(instructions, 0, 2)
      parameters should_== List(2, 3)
    }

    "return 2 position values" in {
      val instructions = Vector(5, 0, 1)
      val parameters = IntcodeComputer.getParameters(instructions, 0, 2)
      parameters should_== List(5, 0)
    }
    "return a position and an immediate" in {
      val instructions = Vector(1005, 2, 1)
      val parameters = IntcodeComputer.getParameters(instructions, 0, 2)
      parameters should_== List(1, 1)
    }
  }

  def withComputer[A](input: Vector[Int],
                      inputValue: List[Int] = List.empty)(test: Computer => A): A = {
    val computer = Computer(0, input, Vector.empty)
    val output = IntcodeComputer.executeInstructions(computer, inputValue)
    test(output)
  }

  "Intcode computer execution" should {
    "Add two numbers and store it in the 5th position" in {
      val instructions = Vector(1, 2, 3, 5, 99, 3)
      withComputer(instructions) { output =>
        output.instructions(5) should_== 8
      }
    }

    "Add two numbers and store it in the 5th position" in {
      val instructions = Vector(1001, 2, 3, 5, 99, 3)
      withComputer(instructions) { output =>
        output.instructions(5) should_== 6
      }
    }

    "Add two numbers and store it in the 5th position" in {
      val instructions = Vector(1101, 2, 3, 5, 99, 3)
      withComputer(instructions) { output =>
        output.instructions(5) should_== 5
      }
    }
    "Multiply two numbers and store it in the 5th position" in {
      val instructions = Vector(2, 2, 3, 5, 99, 3)
      withComputer(instructions) { output =>
        output.instructions(5) should_== 15
      }
    }

    "Multiply two numbers and store it in the 5th position" in {
      val instructions = Vector(1002, 2, 3, 5, 99, 3)
      withComputer(instructions) { output =>
        output.instructions(5) should_== 9
      }
    }

    "Multiply two numbers and store it in the 5th position" in {
      val instructions = Vector(1102, 2, 3, 5, 99, 3)
      withComputer(instructions) { output =>
        output.instructions(5) should_== 6
      }
    }

    "Input one stores it in the 4th position" in {
      val instructions = Vector(3, 3, 99, 4)
      withComputer(instructions, List(8)) { output =>
        output.instructions(3) should_== 8
      }
    }

    "Output one saves the result to the diagnostics output - default position mode" in {
      val instructions = Vector(4, 3, 99, 4)
      withComputer(instructions) { output =>
        output.diagnostics(0) should_== 4
      }
    }
    "Output one saves the result to the diagnostics output - explicit immediate mode" in {
      val instructions = Vector(104, 3, 99, 4)
      withComputer(instructions) { output =>
        output.diagnostics(0) should_== 3
      }
    }

    "Jump if true should do nothing if first parameter is 0" in {
      val instructions = Vector(5, 2, 0, 104, 1, 99, 4)
      withComputer(instructions) { output =>
        output.diagnostics(0) should_== 1
      }
    }
    "Jump if true should do nothing if first parameter is 0 - immediate mode" in {
      val instructions = Vector(1105, 0, 0, 104, 1, 99, 4)
      withComputer(instructions) { output =>
        output.diagnostics(0) should_== 1
      }
    }
    "Jump if true should move instructionPointer if first parameter is not 0 and output 0" in {
      val instructions = Vector(5, 2, 3, 4, 104, 0, 99, 4)
      withComputer(instructions) { output =>
        output.diagnostics(0) should_== 0
      }
    }
    "Jump if true should move instructionPointer if first parameter is not 0 and output 0 - immediate mode" in {
      val instructions = Vector(1105, 20, 4, 4, 104, 0, 99, 4)
      withComputer(instructions) { output =>
        output.diagnostics(0) should_== 0
      }
    }

    "Jump if false should do nothing if first parameter is not 0, and then output 1" in {
      val instructions = Vector(6, 6, 3, 104, 1, 99, 1)
      withComputer(instructions) { output =>
        output.diagnostics(0) should_== 1
      }
    }

    "Jump if false should do nothing if first parameter is not 0, and then output 1 - immediate mode" in {
      val instructions = Vector(1106, 2, 0, 104, 1, 99)
      withComputer(instructions) { output =>
        output.diagnostics(0) should_== 1
      }
    }

    "Jump if false should move the instructionPointer if first parameter is 0, and then output 0" in {
      val instructions = Vector(6, 5, 3, 4, 104, 0, 99)
      withComputer(instructions) { output =>
        output.diagnostics(0) should_== 0
      }
    }

    "Jump if false should move the instructionPointer if first parameter is 0, and then output 0 - immediate mode" in {
      val instructions = Vector(1106, 0, 3, 104, 0, 99)
      withComputer(instructions) { output =>
        output.diagnostics(0) should_== 0
      }
    }

    "Less than should store 1 in the position provided if the first parameter < second parameter" in {
      val instructions = Vector(7, 5, 6, 6, 99, 2, 3)
      withComputer(instructions) { output =>
        output.instructions(6) should_== 1
      }
    }

    "Less than should store 1 in the position provided if the first parameter < second parameter - immediate mode" in {
      val instructions = Vector(1107, 2, 3, 6, 99, 2, 3)
      withComputer(instructions) { output =>
        output.instructions(6) should_== 1
      }
    }

    "Less than should store 0 in the position provided if the first parameter > second parameter" in {
      val instructions = Vector(7, 5, 6, 6, 99, 3, 2)
      withComputer(instructions) { output =>
        output.instructions(6) should_== 0
      }
    }
    "Less than should store 0 in the position provided if the first parameter > second parameter - immediate mode" in {
      val instructions = Vector(1107, 3, 2, 6, 99, 2, 3)
      withComputer(instructions) { output =>
        output.instructions(6) should_== 0
      }
    }

    "Equals than should store 1 in the position provided if the first parameter = second parameter" in {
      val instructions = Vector(8, 5, 6, 6, 99, 2, 2)
      withComputer(instructions) { output =>
        output.instructions(6) should_== 1
      }
    }

    "Equals should store 1 in the position provided if the first parameter = second parameter - immediate mode" in {
      val instructions = Vector(1108, 2, 2, 6, 99, 2, 3)
      withComputer(instructions) { output =>
        output.instructions(6) should_== 1
      }
    }

    "Equals should store 0 in the position provided if the first parameter != second parameter" in {
      val instructions = Vector(8, 5, 6, 6, 99, 3, 2)
      withComputer(instructions) { output =>
        output.instructions(6) should_== 0
      }
    }
    "Equals should store 0 in the position provided if the first parameter != second parameter - immediate mode" in {
      val instructions = Vector(1108, 3, 2, 6, 99, 2, 3)
      withComputer(instructions) { output =>
        output.instructions(6) should_== 0
      }
    }

    "Giving multiple inputs should output those values" in {
      val instructions = Vector(3,5,3,6,99,-1,-1)
      val inputs = List(5,8)

      withComputer(instructions, inputs) { output =>
        output.instructions(5) should_== 5
        output.instructions(6) should_== 8
      }
    }


  }
}
