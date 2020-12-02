object PasswordManager {

  case class Password(
   minRepetitions: Int,
   maxRepetitions: Int,
   repetitionChar: Char,
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
    val password = processPassword(validPassword)
    val occurrences = password.password.count(_ == password.repetitionChar)

    occurrences >= password.minRepetitions && occurrences <= password.maxRepetitions
  }

  def countValidPasswords(passwords: List[String]): Int = {
    passwords.count(checkPassword)
  }

}
