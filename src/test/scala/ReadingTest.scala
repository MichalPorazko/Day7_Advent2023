class ReadingTest extends munit.FunSuite {

  test("testing the toHand method"){
    val hand = "32T3K 765"
    println(ReadingFile.toHand(hand))

  }

  test("testing reading from a file"){
    val part1 = CamelCards()
    val camelCards = ReadingFile.readFile("testFile", part1)
    println(camelCards)
  }

}
