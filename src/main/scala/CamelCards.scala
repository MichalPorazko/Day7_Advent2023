import HandType.{FiveKind, FourKind, FullHouse, HighCard, OnePair, ThreeKind, TwoPair}


case class CamelCards(
                       highCards: List[Hand] = List.empty,
                       onePair: List[Hand] = List.empty,
                       twoPair: List[Hand] = List.empty,
                       threeKind: List[Hand] = List.empty,
                       fullHouse: List[Hand] = List.empty,
                       fourKind: List[Hand] = List.empty,
                       fiveKind: List[Hand] = List.empty)

object CamelCards{

  def addToType(hand: Hand, part1: CamelCards): CamelCards =
    Hand.handType(hand) match
      case FiveKind =>
        part1.copy(fiveKind = hand :: part1.fiveKind)
      case FourKind =>
        part1.copy(fourKind = hand :: part1.fourKind)
      case FullHouse =>
        part1.copy(fullHouse = hand :: part1.fullHouse)
      case ThreeKind =>
        part1.copy(threeKind = hand :: part1.threeKind)
      case TwoPair =>
        part1.copy(twoPair = hand :: part1.twoPair)
      case OnePair =>
        part1.copy(onePair = hand :: part1.onePair)
      case HighCard =>
        part1.copy(highCards = hand :: part1.highCards)

  def handsInOrder(camelCards: CamelCards): List[List[Hand]] = List(
    camelCards.highCards,
    camelCards.onePair,
    camelCards.twoPair,
    camelCards.threeKind,
    camelCards.fullHouse,
    camelCards.fourKind,
    camelCards.fiveKind
  ).filter(_.nonEmpty)

  def rankInList(currentRank: Int, hands: List[Hand]) =
    val sortedHands = hands.sorted(using Hand.handOrdering)
    println(s"The sorted hands: ${sortedHands}")
    val winnings = sortedHands.zipWithIndex.map((hand, index) =>
      println(s"Hand bid its ${hand.bid} index is ${index} current Rank is ${currentRank}")
      println(s"The sum of ${hand.bid} * (${index} + ${currentRank}) is ${hand.bid * (index + currentRank)}")
      hand.bid * (index + currentRank)
    ).sum
    val newRank = currentRank + hands.length
    (winnings, newRank)

  def rankAllHands(camelCards: CamelCards): Int =
    val handsByType = handsInOrder(camelCards)
    /**
     * First Element:
     * The cumulative value (e.g., a list of (Hand, Int) pairs representing hands and their assigned ranks
     * , or an Int representing the total sum).
     *
     * (sum, currentRank) as your accumulator
     *  and
     * hands as your current element
     *
     *
     * so that is why there is just a pair of ints as the accumulator, without a value of List[Hand]
     *
     *the reason why the List[Hand] is in the accumulating value
     * handsByType.foldLeft((List.empty[(Hand, Int)], 1))
     * is that is used as a returned value in the method
     * { case ((accumulatedHands: List[(Hand, Int)], currentRank), handsOfType) =>
     * */
    val (totalWinnings, _) = handsByType.foldLeft[(Int, Int)](0,1) {
      /**
       * case word is needed because teh accumulator is a tuple, so you need to tell the compiler that you
       * are deconstructing this tuple
       *
       * another way to approach this problem without using the case word is
       * handsByType.foldLeft[(Int, Int)](0,0) { (accumulator, hands) =>
       *  val (sum, currentRank) = accumulator
       * ....
       * }
       * */
      case ((winnings, currentRank), hands) =>
        val (updatedWinnings, newRank) = rankInList(currentRank, hands)
        println(s"The updated winnings are ${updatedWinnings} and the new rank is ${newRank}")
        (winnings + updatedWinnings, newRank)
    }
    totalWinnings

  def

}
