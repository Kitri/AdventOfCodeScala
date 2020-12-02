import org.specs2.mutable.Specification

class PasswordManagerSpec extends Specification {

  "Password Manager" should {
    "Check split logic" in {
      val password = "1-3 a: abcde"
      val p = PasswordManager.processPassword(password)

      p.minRepetitions should_== 1
      p.maxRepetitions should_== 3
      p.repetitionChar should_== 'a'
      p.password should_== "abcde"
    }

    "Check if password is valid" in {
      val password = "1-3 a: abcde"
      PasswordManager.checkPassword(password) should_== true
    }

    "Check if password is invalid - minimum" in {
      val password = "1-3 a: bbcde"
      PasswordManager.checkPassword(password) should_== false
    }

    "Check if password is invalid - maximum" in {
      val password = "1-3 a: aaaabbcde"
      PasswordManager.checkPassword(password) should_== false
    }

    "Check list for valid password" in {
      val input = List( "1-3 a: abcde", "1-3 b: cdefg"," 2-9 c: ccccccccc" )
      PasswordManager.countValidPasswords(input) should_== 2
    }
  }

}
