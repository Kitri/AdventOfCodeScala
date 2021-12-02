import org.specs2.mutable.Specification

import scala.concurrent.Future

class TestSpec extends Specification {

  "Anything" should {
    "do what I want" in {

      val x = List(List(0,1,2,2), List(1,0,1,0)) //0,1,1,0

      val foldedness = x.foldLeft(x(0)){(x,y) => {
        for {
          (top, second) <- x zip y
        } yield if(top == 2) second else top
      }}

//      println(s"----------------$foldedness---------------")

      1 should_== 1
    }


    "make coffee" in {
      1 should_== 1
    }

  }
}

object Coffee {

  type BoiledWater
  type ColdWater

  sealed trait Powder
  case object ChicoryPowder extends Powder
  case object GroundCoffee extends Powder

  case class Brew(powder: Powder)

  def combineWaterAndPowder(water: BoiledWater, powder: Powder): Brew = ???
  def brewCoffee(water: BoiledWater, powder: Powder): Brew = ???

  def boil(water: ColdWater): Future[BoiledWater] = ???

  def brew(water: ColdWater, powder: Powder): Future[Brew] = for {
    boiled: BoiledWater <- boil(water)
    brew <- {
      powder match {
        case ChicoryPowder => combineWaterAndPowder(boiled, powder)
        case GroundCoffee => brewCoffee(boiled, powder)
      }
    }
  } yield brew

  case class Cup(water:Future[BoiledWater], sugar: Option[Int], milk: Option[Int], powder: Powder)

  object Cup {
    def apply(water: Future[BoiledWater], sugar: Option[Int], milk: Option[Int], powder: Powder): Cup = {
      new Cup(water, sugar, milk, powder)
    }
  }



  def MakeCoffeeFlavouredDrink(
      water: ColdWater,
      sugar: Option[Int],
      milk: Option[Int],
      powder: Powder
     ): Cup = {

    Cup(boil(water), sugar, milk, powder)

  }



}
