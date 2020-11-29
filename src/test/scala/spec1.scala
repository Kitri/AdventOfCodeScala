import org.specs2.mutable.Specification

class spec1 extends Specification {

  "test1" should {
    "equal hello world" in {

      day1.funky1 should_== "Hello World"

    }

    "do something else " in {
      1 should_== 1
    }
  }
}