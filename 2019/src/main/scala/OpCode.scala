sealed trait OpCode
case object Halt extends OpCode
case object Add extends OpCode
case object Multiply extends OpCode
case object InputOne extends OpCode
case object OutputOne extends OpCode
case object JumpIfTrue extends OpCode
case object JumpIfFalse extends OpCode
case object LessThan extends OpCode
case object Equals extends OpCode

object OpCode {
  def getOpCode(code: Int): OpCode = {
    val codeString = code.toString
    val opcode =
      if (codeString.length < 2) code
      else {
        codeString.splitAt(codeString.length - 2)._2.toInt
      }
    opcode match {
      case 99 => Halt
      case 1  => Add
      case 2  => Multiply
      case 3  => InputOne
      case 4  => OutputOne
      case 5  => JumpIfTrue
      case 6  => JumpIfFalse
      case 7  => LessThan
      case 8  => Equals
    }
  }

  def getParameterModes(code: Int): List[ParameterMode] = {
    val codeString = code.toString
    if (codeString.length < 2) List.empty
    else {
      codeString
        .splitAt(codeString.length - 2)
        ._1
        .toList
        .map(_.asDigit)
        .reverse
        .map(p => ParameterMode.getParameterMode(p))
    }
  }

}
