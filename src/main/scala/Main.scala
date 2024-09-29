object Main extends App{

  import Hand._
  val camelCards = ReadingFile.readFile("file")

  {
    implicit val part1Ordering: CompareLetter = Hand.compareLetter
    implicit val defineTypeOperation: Hand => HandType = Hand.handType
    val part1 = Hand.calculateWinnings(camelCards)
    println(s"The whole sum for part two is ${part1}")
  }

  {
    implicit val part2Ordering: CompareLetter = Hand.compareLetterJ
    implicit val defineTypeOperation: Hand => HandType = Hand.handTypeWithJ
    val part2 = Hand.calculateWinnings(camelCards)
    println(s"The whole sum for part two is ${part2}")
  }




}
