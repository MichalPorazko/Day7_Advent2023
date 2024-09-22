object Main extends App {

  val part1 = CamelCards()
  val la = ReadingFile.readFile("file", part1)

  println(s"The whole sum is ${CamelCards.rankAllHands(la)}")

}
