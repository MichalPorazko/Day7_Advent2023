type Card = Char
type Hand = String

case class Bet(hand: Hand, bid: Int)
object Bet:
  def apply(s: String): Bet = Bet(s.take(5), s.drop(6).toInt)

enum HandType:
  case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
object HandType:
  def apply(hand: Hand)(using rules: Rules): HandType =
    val cardCounts: Map[Card, Int] =
      hand.groupBy(identity).mapValues(_.length).toMap

    val cardGroups: List[Int] = rules.wildcard match
      case Some(card) if cardCounts.keySet.contains(card) =>
        val wildcardCount = cardCounts(card)
        val cardGroupsNoWildcard = cardCounts.removed(card).values.toList.sorted.reverse
        cardGroupsNoWildcard match
          case Nil => List(wildcardCount)
          case _ => cardGroupsNoWildcard.head + wildcardCount :: cardGroupsNoWildcard.tail
      case _ => cardCounts.values.toList.sorted.reverse

    cardGroups match
      case 5 :: _ => HandType.FiveOfAKind
      case 4 :: _ => HandType.FourOfAKind
      case 3 :: 2 :: Nil => HandType.FullHouse
      case 3 :: _ => HandType.ThreeOfAKind
      case 2 :: 2 :: _ => HandType.TwoPair
      case 2 :: _ => HandType.OnePair
      case _ => HandType.HighCard
  end apply
end HandType

trait Rules:
  val rankValues: String
  val wildcard: Option[Card]

val standardRules = new Rules:
  val rankValues = "23456789TJQKA"
  val wildcard = None

val jokerRules = new Rules:
  val rankValues = "J23456789TQKA"
  val wildcard = Some('J')


given cardOrdering(using rules: Rules): Ordering[Card] = Ordering.by(rules.rankValues.indexOf(_))
given handOrdering(using Rules): Ordering[Hand] = (h1: Hand, h2: Hand) =>
  val h1Type = HandType(h1)
  val h2Type = HandType(h2)
  if h1Type != h2Type then h1Type.ordinal - h2Type.ordinal
  else h1.zip(h2).find(_ != _).map( (c1, c2) => cardOrdering.compare(c1, c2) ).getOrElse(0)
given betOrdering(using Rules): Ordering[Bet] = Ordering.by(_.hand)

def calculateWinnings(bets: List[Bet])(using Rules): Int =
  bets.sorted.zipWithIndex.map { case (bet, index) => bet.bid * (index + 1) }.sum

def parse(input: String): List[Bet] =
  input.linesIterator.toList.map(Bet(_))

def part1(input: String): Unit =
  println(calculateWinnings(parse(input))(using standardRules))

def part2(input: String): Unit =
  println(calculateWinnings(parse(input))(using jokerRules))