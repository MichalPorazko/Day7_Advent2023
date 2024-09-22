class CamelCardsTest extends munit.FunSuite {

  test("testing the list adding"){
    val hands = List(
      Hand("32T3K", 765),
      Hand("T55J5", 684),
      Hand("KK677", 28),
      Hand("KTJJT", 220),
      Hand("QQQJA", 483)
    )

    val camelCards = CamelCards()
    val part1 = hands.foldLeft(camelCards)( (part1, hand) => CamelCards.addToType(hand, part1))
    println(s"The three kind hands ${part1.threeKind}")
    println(s"The two pair hands ${part1.twoPair}")
    println(s"The one pair hands ${part1.onePair}")

  }

  test("testing the rankInList method"){

    val threeKindHands = List(
      Hand("33T3K", 10),
      Hand("T55J5", 10)
    )
    /**
     * let's assume that there where there hands of type High Kind and two hands of type One Pair
     * so the ranking of the type ''Three of a Kind'' starts from the number 5 + 1 = 6
     * */

    val currentRank = 6
    val (winnings, newRank) = CamelCards.rankInList(currentRank, threeKindHands)
    println(s"The winnings should be 10 * 6 + 7 * 10 = 130 and its ${winnings} and the new Rank should be 8 and its ${newRank}")


  }

  test("testing the handsInOrder method"){
    val hands = List(
      Hand("32T3K", 765),
      Hand("T55J5", 684),
      Hand("KK677", 28),
      Hand("KTJJT", 220),
      Hand("QQQJA", 483)
    )

    val camelCards = CamelCards()
    val part1 = hands.foldLeft(camelCards)((part1, hand) => CamelCards.addToType(hand, part1))
    println(part1)
    println(s"The camel cards after the handsInOrder method:\n ${CamelCards.handsInOrder(part1)}")
  }

  test("testing the rankAllHands method"){
    val hands = List(
      Hand("32T3K", 765),
      Hand("T55J5", 684),
      Hand("KK677", 28),
      Hand("KTJJT", 220),
      Hand("QQQJA", 483)
    )

    val camelCards = CamelCards()
    val part1 = hands.foldLeft(camelCards)((part1, hand) => CamelCards.addToType(hand, part1))

    println(CamelCards.rankAllHands(part1))
  }

}
