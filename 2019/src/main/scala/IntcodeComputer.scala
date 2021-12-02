import scala.annotation.tailrec

case class Computer(instructionPointer: Int,
                    instructions: Vector[Int],
                    diagnostics: Vector[Int],
                    status: ProgramStatus = Running) {
  def resetStatus: Computer = {
    Computer(instructionPointer, instructions, diagnostics.empty)
  }
}

sealed trait ProgramStatus
case object Halted extends ProgramStatus
case object Paused extends ProgramStatus
case object Running extends ProgramStatus

object IntcodeComputer {
  private val lastParameterIsNotAnAddress =
    List(OutputOne, JumpIfFalse, JumpIfTrue)

  def getParameterValue(instructionPointer: Int,
                        instructions: Vector[Int],
                        index: Int,
                        parameterModes: List[ParameterMode]): Int = {
    val instructionIndex = instructionPointer + index
    if (parameterModes.nonEmpty
        && index < parameterModes.size
        && parameterModes(index) == Immediate)
      instructions(instructionIndex)
    else
      instructions(instructions(instructionIndex))
  }

  def getParameters(instructions: Vector[Int],
                    instructionPointer: Int,
                    nrOfParameters: Int): List[Int] = {
    val currentInstruction = instructions(instructionPointer)
    val currentOpcode = OpCode.getOpCode(currentInstruction)
    val parameterModes =
      OpCode.getParameterModes(currentInstruction)
    (0 until nrOfParameters)
      .map(index => {
        if (!lastParameterIsNotAnAddress
              .contains(currentOpcode) && index == (nrOfParameters - 1))
          instructions(instructionPointer + 1 + index)
        else
          getParameterValue(
            instructionPointer + 1,
            instructions,
            index,
            parameterModes
          )
      })
      .toList

  }

  def execute(instructions: Vector[Int], instructionPointer: Int)(
    nrOfParameters: Int
  )(newInstructionPointer: List[Int] => Int)(
    updatedInstructions: List[Int] => Vector[Int]
  )(updatedDiagnostics: List[Int] => Vector[Int]
  )(status: ProgramStatus): Computer = {

    val parameters =
      getParameters(instructions, instructionPointer, nrOfParameters)

    Computer(
      newInstructionPointer(parameters),
      updatedInstructions(parameters),
      updatedDiagnostics(parameters),
      status
    )
  }

  def executeInstruction(computer: Computer, input: List[Int] = List.empty): Computer = {
    val instructions = computer.instructions
    val instructionPointer = computer.instructionPointer
    val opcode = OpCode.getOpCode(instructions(instructionPointer))

    val executeOperation: Int => (List[Int] => Int) => (List[Int] => Vector[Int]) => (List[Int] =>
      Vector[Int]) => ProgramStatus => Computer =
      execute(instructions, instructionPointer)

    val updatedComputer = opcode match {
      case Halt => Computer(instructionPointer, instructions, computer.diagnostics, Halted)
      case Add => {
        executeOperation(3) { _ =>
          instructionPointer + 4
        } { parameters =>
          instructions.updated(parameters(2), parameters(0) + parameters(1))
        } { _ =>
          computer.diagnostics
        }(Running)
      }

      case Multiply => {
        executeOperation(3) { _ =>
          instructionPointer + 4
        } { parameters =>
          instructions.updated(parameters(2), parameters(0) * parameters(1))
        } { _ =>
          computer.diagnostics
        }(Running)
      }

      case InputOne => {
        if(input.isEmpty)
          Computer(instructionPointer, instructions, computer.diagnostics, Paused)
        else {
          executeOperation(1) { _ =>
            instructionPointer + 2
          } { parameters =>
            instructions.updated(parameters(0), input.head)
          } { _ =>
            computer.diagnostics
          }(Running)
        }
      }

      case OutputOne => {
        executeOperation(1) { _ =>
          instructionPointer + 2
        } { _ =>
          instructions
        } { parameters =>
          computer.diagnostics.appended(parameters(0))
        }(Running)
      }

      case JumpIfTrue => {
        executeOperation(2) { parameters =>
          if (parameters(0) == 0) instructionPointer + 3
          else parameters(1)
        } { _ =>
          instructions
        } { _ =>
          computer.diagnostics
        }(Running)
      }

      case JumpIfFalse => {
        executeOperation(2) { parameters =>
          if (parameters(0) == 0) parameters(1)
          else instructionPointer + 3
        } { _ =>
          instructions
        } { _ =>
          computer.diagnostics
        }(Running)
      }

      case LessThan => {
        executeOperation(3) { _ =>
          instructionPointer + 4
        } { parameters =>
          instructions.updated(
            parameters(2),
            if (parameters(0) < parameters(1)) 1 else 0
          )
        } { _ =>
          computer.diagnostics
        }(Running)
      }

      case Equals => {
        executeOperation(3) { _ =>
          instructionPointer + 4
        } { parameters =>
          instructions.updated(
            parameters(2),
            if (parameters(0) == parameters(1)) 1 else 0
          )
        } { _ =>
          computer.diagnostics
        }(Running)
      }
    }

    updatedComputer
  }

  @tailrec
  def executeInstructions(computer: Computer, input: List[Int]): Computer = {
    val currentOpCode =
      OpCode.getOpCode(computer.instructions(computer.instructionPointer))
    if (computer.status == Halted || computer.status == Paused)
      computer
    else {
      val newInput = if(input.nonEmpty && currentOpCode == InputOne) input.tail else input
      executeInstructions(executeInstruction(computer, input), newInput)
    }
  }
}
