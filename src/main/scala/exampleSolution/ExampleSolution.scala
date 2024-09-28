package exampleSolution

type Card = Char
type Hand = String

case class Bet(hand: Hand, bid: Int)

enum HandType:
  case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

object Bet:
  def apply(s: String): Bet = Bet(s.take(5), s.drop(6).toInt)

object HandType:
  def apply(hand: Hand): HandType =
    val cardCounts: List[Int] =
      hand.groupBy(identity).values.toList.map(_.length).sorted.reverse

    cardCounts match
      case 5 :: _ => HandType.FiveOfAKind
      case 4 :: _ => HandType.FourOfAKind
      case 3 :: 2 :: Nil => HandType.FullHouse
      case 3 :: _ => HandType.ThreeOfAKind
      case 2 :: 2 :: _ => HandType.TwoPair
      case 2 :: _ => HandType.OnePair
      case _ => HandType.HighCard
  end apply

  val ranks = "23456789TJQKA"
  given cardOrdering: Ordering[Card] = Ordering.by(ranks.indexOf(_))
  given handOrdering: Ordering[Hand] = (h1: Hand, h2: Hand) =>
    val h1Type = HandType(h1)
    val h2Type = HandType(h2)
    if h1Type != h2Type then h1Type.ordinal - h2Type.ordinal
    else h1.zip(h2).find(_ != _).map((c1, c2) => cardOrdering.compare(c1, c2)).getOrElse(0)

  given betOrdering: Ordering[Bet] = Ordering.by( bet => bet.hand)

