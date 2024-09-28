
class ExampleSolution extends munit.FunSuite {

  test("testing the apply method of object") {
    val string = "32T3K 24"

    println(s"The function s.take(5) returns ${string.take(5)}")
    println(s"The function s.take(7) returns ${string.take(7)}")
    println(s"The function s.drop(6) returns ${string.drop(6)}")

    assertEquals(Bet.apply(string), Bet("32T3K", 24))
  }

  test("testing the orderings in the example solution"){

    val ranks = "23456789TJQKA"
    println(s"The index of K is ${ranks.indexOf('K')} \n or the index of T is ${ranks.indexOf('T')}" )
    given cardOrdering: Ordering[Card] = Ordering.by(index => ranks.indexOf(index))

    enum HandType:
      case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

    println(s"FiveOfAKind.ordinal - FullHouse.ordinal is ${HandType.FiveOfAKind.ordinal - HandType.FullHouse.ordinal}")

    val hand1 = "QQQJA"
    val hand2 = "KTJJT"
    val hand3 = "TTTTT"
    val hand4 = "TTTTT"
    println(s"result of h1.zip(h2) is ${hand1.zip(hand2)}")

    /**
     * Returns:
     * an option value containing the first element in the collection
     * that satisfies predicate, or None if none exists.
     * */

    println(s"using the find function on zipping two hands: ${hand1.zip(hand2).find(_ != _ )}")

    println(s"comparing hands QQQJA and KTJJT according to card ordering: ${hand1.zip(hand2).find(_ != _ ).map(cardOrdering.compare(_, _))}")

    println(hand3.zip(hand4).find(_ != _))
    println(hand3.zip(hand4).find(_ != _).map(cardOrdering.compare(_, _)).getOrElse(0))
  }

  test("Changing my solution"){


  }

}
