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
    val part1 = hands.foldLeft(camelCards)( (part1, hand) => CamelCards.addToType(hand, part1, Hand.handType))
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
     * let's assume that there were there hands of type High Kind and two hands of type One Pair
     * so the ranking of the type ''Three of a Kind'' starts from the number 5 + 1 = 6
     * */

    val currentRank = 6
    import Hand._
    implicit val ordering: CompareLetter = Hand.compareLetter
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
    val part1 = hands.foldLeft(camelCards)((part1, hand) => CamelCards.addToType(hand, part1, Hand.handType))
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
    val part1 = hands.foldLeft(camelCards)((part1, hand) => CamelCards.addToType(hand, part1, Hand.handType))

    import Hand._
    implicit val ordering: CompareLetter = Hand.compareLetter
    println(CamelCards.rankAllHands(part1))
  }

  test("testing camel Cards based on containing a'J"){

    val hands = List(
      Hand("32T3K", 765),
      Hand("T55J5", 684),
      Hand("KK677", 28),
      Hand("KTJJT", 220),
      Hand("QQQJA", 483)
    )

    val camelCards = CamelCards()
    val part1 = hands.foldLeft(camelCards)((part1, hand) => CamelCards.addToType(hand, part1, Hand.handType))

    val filteredCC = CamelCards.filterCamelCards(part1)
    println(filteredCC)

  }

  test("testing the rankInList method for the 'J"){
    val fourKindHands = List(
      Hand("QQQJA", 483),
      Hand("T55J5", 10),
      Hand("KTJJT", 220)
    )

    val currentRank = 3
    import Hand._
    implicit val ordering: CompareLetter = Hand.compareLetterJ
    println(fourKindHands.sorted)
    val (winnings, newRank) = CamelCards.rankInList(currentRank, fourKindHands)
    println(s"The winnings should be 3 * 684 + 4 * 483 + 5 * 220 = 5804 and its ${winnings} and the new Rank should be 6 and its ${newRank}")
  }

  test("testing the rankAllHands method for 'J' on a bigger list"){
    val hands = List(
      Hand("324KJ", 12),  // High Card
      Hand("Q29KA", 14),
      Hand("32T3K", 765), // One Pair
      Hand("J54AQ", 200),
      Hand("QQJJ2", 234), // Two Pair
      Hand("KK8AA", 212),
      Hand("KK677", 28),
      Hand("TJ555", 684), // Three of a Kind
      Hand("KTJJJ", 220),
      Hand("QQQ9A", 483),
      Hand("KK777", 10),  // Full House
      Hand("9A9A9", 128),
      Hand("222JJ", 5),
      Hand("KKKK6", 28),  // Four of a Kind
      Hand("JJJJ8", 100),
      Hand("TTTTT", 28),  // Five of a Kind
      Hand("JJJJJ", 15)
    )

    val camelCards = CamelCards()
    val part1 = hands.foldLeft(camelCards)((part1, hand) => CamelCards.addToType(hand, part1, Hand.handType))

    // Print each list from CamelCards before filtering
    println("CamelCards high cards before filtering: " + part1.highCards.mkString(", "))
    println("CamelCards one pair before filtering: " + part1.onePair.mkString(", "))
    println("CamelCards two pair before filtering: " + part1.twoPair.mkString(", "))
    println("CamelCards three of a kind before filtering: " + part1.threeKind.mkString(", "))
    println("CamelCards full house before filtering: " + part1.fullHouse.mkString(", "))
    println("CamelCards four of a kind before filtering: " + part1.fourKind.mkString(", "))
    println("CamelCards five of a kind before filtering: " + part1.fiveKind.mkString(", "))

    val filteredWithJ = CamelCards.filterCamelCards(part1)

    // Print each list from CamelCards after filtering
    println("CamelCards high cards after filtering: " + filteredWithJ.highCards.mkString(", "))
    println("CamelCards one pair after filtering: " + filteredWithJ.onePair.mkString(", "))
    println("CamelCards two pair after filtering: " + filteredWithJ.twoPair.mkString(", "))
    println("CamelCards three of a kind after filtering: " + filteredWithJ.threeKind.mkString(", "))
    println("CamelCards full house after filtering: " + filteredWithJ.fullHouse.mkString(", "))
    println("CamelCards four of a kind after filtering: " + filteredWithJ.fourKind.mkString(", "))
    println("CamelCards five of a kind after filtering: " + filteredWithJ.fiveKind.mkString(", "))

    import Hand._
    implicit val ordering: CompareLetter = Hand.compareLetterJ
    println(s"Total winnings after ranking all hands: ${CamelCards.rankAllHands(filteredWithJ)}")
  }



}
