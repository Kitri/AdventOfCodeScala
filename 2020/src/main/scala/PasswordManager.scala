object PasswordManager {

  case class Password(
   index1: Int,
   index2: Int,
   char: Char,
   password: String
  )

  def processPassword(validPassword: String): Password = {
    //Password structure: 1-3 a: password
    val pwSplit = validPassword.split(' ')
    if(pwSplit.length == 3) {
      val numbers = pwSplit.head.split('-')
      val char = pwSplit(1).split(':').head.toCharArray.head
      val pass = pwSplit(2)

      Password(numbers(0).toInt, numbers(1).toInt, char, pass)

    }
    else Password(0,0,' ', "")
  }

  def checkPassword(validPassword: String): Boolean = {
    val pass = processPassword(validPassword)

    val first = pass.password.charAt(pass.index1-1) == pass.char
    val second = pass.password.charAt(pass.index2-1) == pass.char

    first ^ second

    /*
        first   |  second  | expectedOutcome
          1         1           0
          1         0           1
          0         1           1
          0         0           0

     */

  }

  def countValidPasswords(passwords: List[String]): Int = {
    passwords.count(checkPassword)
  }

}
