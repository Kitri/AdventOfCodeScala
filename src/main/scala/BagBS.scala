object BagBS {
  private val fileIn = InputParser.parseInput("day7.txt")
  val bags: List[Bag] = fileIn.map(createRule)

  def part1(): Int = {
    val x = findBagsThatContainThisBag("shiny gold")
    val recurResult = getAllBagsThatCarryShinyGold(x, List.empty).distinct
    recurResult.size
  }

  def part2(): Int = {
    val shinyGold = findBag("shiny gold")
    getAllBagsInsideShinyGold(shinyGold, countBags(shinyGold.bagsInside))
  }

  def getAllBagsInsideShinyGold(bag: Bag, count: Int): Int = {
    if(bag.bagsInside.isEmpty) count
    else {
      val counts = for {
        innerBagMap <- bag.bagsInside
        innerBag = findBag(innerBagMap._1)
        sumOfBags = countBags(innerBag.bagsInside)
      } yield innerBagMap._2 * getAllBagsInsideShinyGold(innerBag, sumOfBags)
      counts.sum + count
    }
  }
  def getAllBagsThatCarryShinyGold(search: List[String], bagCount: List[String]): List[String] = {
    if(search.isEmpty) bagCount
    else {
      for {
        bag <- search
        list <- getAllBagsThatCarryShinyGold(findBagsThatContainThisBag(bag), bagCount :+ bag)
      } yield list
    }
  }

  private def createRule(str: String): Bag = {
    val cleanString = str.replace(" bags","").replace(" bag", "").replace(".", "")

    val split1 = cleanString.split(" contain ")
    val name = split1.head

    if(split1(1) == "no other") Bag(name, Map.empty)
    else {
      val split2 = split1(1).split(", ")
      val m1 = split2.map(makeMap)
      Bag(name, m1.toMap)
    }
  }

  private def makeMap(str: String): (String, Int) = {
    val split1 = str.splitAt(1)
    val numBags = split1._1.trim.toInt
    split1._2.trim -> numBags
  }

  private def findBagsThatContainThisBag(bag: String): List[String] = bags.filter(x => x.bagsInside.contains(bag)).map(_.name)

  private def countBags(bags: Map[String, Int]): Int = {
    if(bags.isEmpty) 0
    else bags.values.sum
  }
  private def namesToBags(names: List[String]): List[Bag] = {
    names.map(findBag)
  }
  private def findBag(name: String): Bag = {
    bags.filter(_.name == name).head
  }

  case class Bag ( name: String, bagsInside: Map[String, Int] )


  // If I need to test this crap again...
  val input =List(
    "light red bags contain 1 bright white bag, 2 muted yellow bags",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags",
    "bright white bags contain 1 shiny gold bag",
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags",
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags", //3
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags", //7
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags", //11 * 2
    "faded blue bags contain no other bags",
    "dotted black bags contain no other bags",
    "some pink bags contain 1 bright white bag",
    "some yellow bags contain 2 muted yellow bags"
    //22 + 7 + 3
  )
}
