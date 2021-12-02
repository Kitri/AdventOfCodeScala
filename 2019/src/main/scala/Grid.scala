object Grid {

  def determinePath(instruction: String,
                    startingPoint: GridPosition): List[GridPosition] = {
    val split = instruction.splitAt(1)
    val direction = getDirection(split._1)
    val distance = 1 to split._2.toInt

    val movementPath = direction match {
      case Up =>
        distance
          .map(d => GridPosition(startingPoint.row - d, startingPoint.column))
      case Down =>
        distance
          .map(d => GridPosition(startingPoint.row + d, startingPoint.column))
      case Left =>
        distance
          .map(d => GridPosition(startingPoint.row, startingPoint.column - d))
      case Right =>
        distance
          .map(d => GridPosition(startingPoint.row, startingPoint.column + d))
    }

    movementPath.toList
  }

  def getDirection(value: String): Direction = {
    value match {
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
    }
  }

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  case class GridPosition(row: Int, column: Int)
}
