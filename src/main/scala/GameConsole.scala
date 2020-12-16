object GameConsole {

  def getAccValue(input: List[String]): Int = {
    val instructions = processInstructions(input)
    process(instructions, instructions.head, 0, List.empty)

  }

  def process(instructions: List[Instruction], instruction: Instruction, acc: Int, indexesProcessed: List[Int]): Int = {
    if(indexesProcessed.contains(instruction.index)) acc
    else {
      val index = instruction.index
      instruction.instruction match {
        case "nop" => process(instructions, instructions(index + 1), acc, indexesProcessed :+ index)
        case "acc" => process(instructions, instructions(index + 1), adjustAcc(acc, instruction), indexesProcessed :+ index)
        case "jmp" => process(instructions, instructions(getJumpIndex(index, instruction)), acc, indexesProcessed :+ index)
      }
    }
  }

  def getJumpIndex(index: Int, instruction: Instruction): Int = {
    if(instruction.direction == "+") index + instruction.amount
    else index - instruction.amount
  }

  def adjustAcc(acc: Int, instruction: Instruction): Int = {
    if(instruction.direction == "+") acc + instruction.amount
    else acc - instruction.amount
  }

  def processInstructions(input: List[String]): List[Instruction] = {
    input.zipWithIndex.map{ case (inst, index) =>
      val x = inst.split(" ")
      val y = x(1).splitAt(1)
      Instruction(index, x.head, y._1, y._2.toInt)
    }
  }
  case class Instruction( index: Int, instruction: String, direction: String, amount: Int)

}
