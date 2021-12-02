sealed trait ParameterMode
case object Position extends ParameterMode
case object Immediate extends ParameterMode

object ParameterMode {

  def getParameterMode(code: Int): ParameterMode = {
    code match {
      case 0 => Position
      case 1 => Immediate
    }
  }
}
