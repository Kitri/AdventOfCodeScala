object GameConsole {

  def findPossibleInstructions(allInstructions: List[Instruction], instructions: List[Instruction], listy: List[List[Instruction]]): List[List[Instruction]] = instructions match {
    case Nil => listy
    case head :: tail =>
      if(head.instruction == "jmp") {
        val newInstructions = allInstructions.updated(head.index, Instruction(head.index, "nop", head.direction, head.amount))
        findPossibleInstructions(allInstructions, tail, listy :+ newInstructions)
      }
      else if(head.instruction == "nop") {
        val newInstructions = allInstructions.updated(head.index, Instruction(head.index, "jmp", head.direction, head.amount))
        findPossibleInstructions(allInstructions, tail, listy :+ newInstructions)
      }
      else
        findPossibleInstructions(allInstructions, tail, listy)
  }

  def getAccValue(input: List[String]): Int = {
    val instructions = processInstructions(input)
    val newInstructions = findPossibleInstructions(instructions, instructions, List.empty)
    val xx = newInstructions.map( process(_, 0, 0, List.empty) )
    xx.filter(_ != -1).head
  }

  def process(instructions: List[Instruction], index: Int, acc: Int, indexesProcessed: List[Int]): Int = {
    if(index >= instructions.size || index < 0) acc
    else if(indexesProcessed.contains(index)) -1
    else {
      val instruction = instructions(index)
      instruction.instruction match {
        case "nop" => process(instructions, index + 1, acc, indexesProcessed :+ index)
        case "acc" => process(instructions, index + 1, adjustAcc(acc, instruction), indexesProcessed :+ index)
        case "jmp" => process(instructions, getJumpIndex(index, instruction), acc, indexesProcessed :+ index)
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
