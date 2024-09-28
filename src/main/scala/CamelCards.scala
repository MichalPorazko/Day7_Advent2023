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

  def addToType(hand: Hand, part1: CamelCards, function: Hand => HandType): CamelCards =
    function(hand) match
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

  def filterCamelCards(camelCards: CamelCards): CamelCards =
    handsInOrder(camelCards).flatten.foldLeft(CamelCards()){(cc, hand) =>
      val  handTypeOperation = if (hand.cards.contains('J')) Hand.handTypeWithJ else Hand.handType
      addToType(hand, cc, handTypeOperation)
    }

  def rankInList(currentRank: Int, hands: List[Hand])(using Ordering[Hand]): (Int, Int) =
    val sortedHands = hands.sorted
    val winnings = sortedHands.zipWithIndex.map((hand, index) =>
      hand.bid * (index + currentRank)
    ).sum
    val newRank = currentRank + hands.length
    (winnings, newRank)

  def rankAllHands(camelCards: CamelCards)(using Ordering[Hand]): Int =
    val handsByType = handsInOrder(camelCards)
    val (totalWinnings, _) = handsByType.foldLeft[(Int, Int)](0,1) {
      case ((winnings, currentRank), hands) =>
        val (updatedWinnings, newRank) = rankInList(currentRank, hands)
        (winnings + updatedWinnings, newRank)
    }
    totalWinnings
}
