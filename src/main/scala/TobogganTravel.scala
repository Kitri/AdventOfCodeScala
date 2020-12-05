object TobogganTravel {

  def countTreesTrajectory(map: List[String]): Int = {
    val multiplyBy = (map.length * 3 / map.head.length) + 1
    val replicated = map.map(x => (1 to multiplyBy).map(_ => x).mkString)
    replicated.zipWithIndex.count {
      case (str, index) =>
        index != 0 && str.charAt(index*3) == '#'
    }
  }
}
