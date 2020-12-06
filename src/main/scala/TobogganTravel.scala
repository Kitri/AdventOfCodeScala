object TobogganTravel {

  def countTreesTrajectory(map: List[String], rightMove: Int, downMove: Int): Int = {
    val multiplyBy = (map.length * rightMove / map.head.length) + 1
    val replicated = map.map(x => (1 to multiplyBy).map(_ => x).mkString)
    replicated.zipWithIndex.count {
      case (str, index) =>
        index != 0 && (index % downMove == 0) && str.charAt(index/downMove*rightMove) == '#'
    }
  }
}
