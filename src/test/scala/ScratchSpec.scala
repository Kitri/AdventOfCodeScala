import org.specs2.mutable.Specification

import scala.collection.immutable

class ScratchSpec extends Specification {
  "Scratch" should {
    "scratch" in {
      val input = List(1721, 979, 366, 299, 675, 1456)
      val combinationsOf2= input.combinations(2).toList
      val combinationsOf3 = input.combinations(3).toList

      val xx= combinationsOf2.filter(_.sum == 2020).head
      print(xx.product)

      val xxx= combinationsOf3.filter(_.sum == 2020).head
      print(xxx.product)
      1 should_== 1
    }
  }
}