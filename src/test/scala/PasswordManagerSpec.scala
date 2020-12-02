import org.specs2.mutable.Specification

class PasswordManagerSpec extends Specification {

  "Password Manager" should {
    "Check split logic" in {
      val password = "1-3 a: abcde"
      val p = PasswordManager.processPassword(password)

      p.index1 should_== 1
      p.index2 should_== 3
      p.char should_== 'a'
      p.password should_== "abcde"
    }

    "Check if password is valid" in {
      val password = "1-3 a: abcde"
      PasswordManager.checkPassword(password) should_== true
    }

    "Check if password is invalid - 0 correct positions" in {
      val password = "1-3 a: bbcde"
      PasswordManager.checkPassword(password) should_== false
    }

    "Check if password is invalid - 2 correct positions" in {
      val password = "1-3 a: aaaaa"
      PasswordManager.checkPassword(password) should_== false
    }

    "Check list for valid password" in {
      val input = List( "1-3 a: abcde", "1-3 b: cdefg","2-9 c: ccccccccc" )
      PasswordManager.countValidPasswords(input) should_== 1
    }
  }

}
