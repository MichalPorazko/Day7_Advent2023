import Hand.charsCount

class HandTest extends munit.FunSuite {

  test("testing the groupBy method "){
    val hand = "TTT98"
    val groupByMethod = hand.groupBy(identity).map((c, s) => (c, s.length))
    println(s"The result of hand.groupBy(identity) is ${hand.groupBy(identity)}")
    assertEquals(groupByMethod, Map('8' -> 1, '9' -> 1, 'T' -> 3))

    val counts = scala.collection.mutable.Map[Char, Int]()
    hand.foreach { card =>
      println(s"counts.getOrElse(card, 0) + 1 result in ${counts.getOrElse(card, 0) + 1}")
      counts(card) = counts.getOrElse(card, 0) + 1
    }
    assertEquals(counts.toList, List(('T',3), ('8',1), ('9',1)))

    val reverseMethod = groupByMethod.toList.reverse
    println(reverseMethod)

    val mapConverse = reverseMethod.map((_, int) => int)
    println(mapConverse)

    val finalMethod = hand.groupBy(identity).map((c, s) => (c, s.length)).toList.reverse
    println(finalMethod)

    val hand1 = Hand("KTJJT", 220)
    val pairs1 = hand1.cards.groupBy(identity).map((c, s) => (c, s.length)).toList
    println(s"the pairs 1 method on the hand 1 : $pairs1")
    

  }

  test("the handType recognising method"){

    val hand1 = Hand("KTJJT", 220)
    val pairsHand1 = Hand.charsCount(hand1)
    println(s"The pairs method on the hand 1: ${pairsHand1}")


    val hands = List(
      Hand("32T3K", 765),
      Hand("T55J5", 684),
      Hand("KK677", 28),
      Hand("KTJJT", 220),
      Hand("QQQJA", 483)
    )
    hands.foreach { hand =>
      val handType = Hand.handType(hand)
      println(s"the hand ${hand} and its type ${handType}")
    }

  }

  test("testing the read function"){
    val input = "32T3K 765"
    val lines = input.split(" ").map(_.trim).toList
    println(lines)

  }

  test("testing the digitStrength map"){
    val digitStrength: Map[Char, Int] = Map(
      'A' -> 13,
      'K' -> 12,
      'Q' -> 11,
      'J' -> 10,
      'T' -> 9,
      '9' -> 8,
      '8' -> 7,
      '7' -> 6,
      '6' -> 5,
      '5' -> 4,
      '4' -> 3,
      '3' -> 2,
      '2' -> 1,
    )

    println(1.compare(2))
    println(2.compare(1))
    println(2.compare(2))

    println(s"A compare to K: ${digitStrength('A').compare(digitStrength('K'))}")
    println(s"A compare to K: ${digitStrength('K').compare(digitStrength('A'))}")
    println(s"A compare to K: ${digitStrength('A').compare(digitStrength('A'))}")
  }

  test("testing the compareHands method"){
    val hand1 = Hand("KTJJT", 220)
    val hand2 = Hand("QQQJA", 483)

    val hands = List(
      Hand("32T3K", 765),
      Hand("T55J5", 684),
      Hand("KK677", 28),
      Hand("KTJJT", 220),
      Hand("QQQJA", 483)
    )

    val zip = hand1.cards zip hand2.cards
    println(zip)

    println(hand1.cards.compare(hand2.cards))
    import Hand._
    implicit val ordering: CompareLetter = Hand.compareLetter
    implicit val defineTypeOperation: Hand => HandType = Hand.handType
    println(hands.sorted(using Hand.handOrdering))

  }

  test("testing the handType method with J"){

    val handsWIthJ = List(
      Hand("JJJJJ", 765),
      Hand("JJJJA", 684),
      Hand("JJJ22", 28),
      Hand("AJJJK", 220),
      Hand("AJAJA", 483),
      Hand("QJ2JQ", 657),
      Hand("J8AJ6", 527),
      Hand("JAAAA", 424),
      Hand("JQQQ6", 352),
      Hand("J55QQ", 513),
      Hand("JAA23", 547),
      Hand("J4567", 993),
      Hand("324KJ", 12),
      Hand("J54AQ", 200)
    )

    handsWIthJ.foreach(hand =>
      println(s"The hand ${hand.cards}, it's old type was ${Hand.handType(hand)} and the new type is ${Hand.handTypeWithJ(hand)} ")
    )

    val hand = Hand("QJ2JQ", 657)
    println(Hand.handTypeWithJ(hand))

  }
  
  test("Ordinal Test"){
    println(HandType.FiveKind.ordinal - HandType.HighCard.ordinal)
  }

  test("testing the handTypeWithJ"){
    val hands = List(
      Hand("32T3K", 765),
      Hand("T55J5", 684),
      Hand("KK677", 28),
      Hand("KTJJT", 220),
      Hand("QQQJA", 483)
    )
    hands.foreach(hand =>
      println(hand)
      val pairs = charsCount(hand)
      val countJ = pairs.getOrElse('J', 0)
      println(countJ)
      val sortedList = (pairs - 'J').toList.sortBy((_, int) => -int).map((_, int) => int)
      println(sortedList)
      println(countJ + sortedList.head) :: sortedList.tail)
  }

}
