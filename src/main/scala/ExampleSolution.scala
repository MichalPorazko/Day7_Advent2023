import math.Ordering.Implicits.* // gives us <, >, == on Ordering[T]s

object DataDefs:
  enum Card:
    case Num(n: Int)
    case Ten
    case Jack
    case Queen
    case King
    case Ace
    lazy val value: Int = this match // part 1
      case Ace    => 14
      case King   => 13
      case Queen  => 12
      case Jack   => 11
      case Ten    => 10
      case Num(n) => n
    lazy val jokerValue: Int = this match // part 2
      case Ace    => 14
      case King   => 13
      case Queen  => 12
      case Ten    => 10
      case Num(n) => n
      case Jack   => 1

  import Card.*
  /**
   * without this 
   * import Card.*
   * you will have a problem with using the Card name
   * just by it's name without the Card addition so
   * Card.Jack
   * */
  given Ordering[Card] with // part 1
    def compare(x: Card, y: Card): Int = x.value - y.value

  enum Rank:
    case HighCard, OnePair, TwoPairs, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
  import Rank.*
  given Ordering[Rank] with // part 1
    def compare(x: Rank, y: Rank): Int = x.ordinal compare y.ordinal

  case class Hand(cards: Seq[Card]):
    lazy val partition = cards
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .toList
      .sorted
    lazy val rank: Rank = partition match
      case List(5)          => FiveOfAKind
      case List(1, 4)       => FourOfAKind
      case List(2, 3)       => FullHouse
      case List(1, 1, 3)    => ThreeOfAKind
      case List(1, 2, 2)    => TwoPairs
      case List(1, 1, 1, 2) => OnePair
      case _                => HighCard

    lazy val isAllJokers = cards.forall(_ == Jack)
    lazy val indexedCards = cards.zipWithIndex
    lazy val (jokers, others) = indexedCards.partition((card, _) => card == Jack)
    //distinct -> Selects all the elements of this sequence ignoring the duplicates.
    lazy val nonJokers = others.map(_._1).distinct

    /**
     * there is nothing bad that the substitutes are a Seq[Seq[(Card, Int)]] because a Hand is a Seq[Hands]
     * so for example a Seq[Hands] is a Seq[Seq[Card]]
     * */

    lazy val substitutes: Seq[Seq[(Card, Int)]] =
      for nonJoker <- nonJokers
        yield jokers.map((_, index) => (nonJoker, index))
    lazy val subbedHands: Seq[Hand] = // replace all Jokers with the same card.
      for sub <- substitutes
        yield Hand((others ++ sub).sortBy(_._2).map(_._1))
    lazy val jokerRank: Rank =
      if isAllJokers then FiveOfAKind else subbedHands.maxBy(_.rank).rank

  given Ordering[Hand] with // part 1
    def compare(x: Hand, y: Hand): Int =
      Ordering[Rank].compare(x.rank, y.rank) match
        case 0 => Ordering[Seq[Card]].compare(x.cards, y.cards)
        case n => n

  case class Bid(hand: Hand, bid: Int)
  given Ordering[Bid] with // part 1
    def compare(x: Bid, y: Bid): Int = Ordering[Hand].compare(x.hand, y.hand)

  object Joker: // part 2: to deal with given import priority order.
    given Ordering[Card] with
      def compare(x: Card, y: Card): Int = x.jokerValue - y.jokerValue

    given Ordering[Hand] with
      def compare(x: Hand, y: Hand): Int =
        Ordering[Rank].compare(x.jokerRank, y.jokerRank) match
          case 0 => Ordering[Seq[Card]].compare(x.cards, y.cards)
          case n => n

    given Ordering[Bid] with
      def compare(x: Bid, y: Bid): Int = Ordering[Hand].compare(x.hand, y.hand)

object Parsing:
  import DataDefs.*, Card.*
  extension (char: Char)
    def toCard: Card = char match
      case 'A' => Ace
      case 'K' => King
      case 'Q' => Queen
      case 'J' => Jack
      case 'T' => Ten
      case n   => Num(n.asDigit)

  def parseHand(hand: String): Hand = Hand(hand.map(_.toCard))
  def lineToBid(line: String): Bid = line match
    case s"$hand $bid" => Bid(parseHand(hand), bid.toInt)

object Solving:
  import DataDefs.*
  def solve(lines: Seq[String])(using Ordering[Bid]): Long = lines
    .map(Parsing.lineToBid)
    .sorted // this is where the magic happens!
    .zipWithIndex
    .map((bid, index) => bid.bid * (index + 1))
    .sum

object Testing:
  val testInput = """
                    |32T3K 765
                    |T55J5 684
                    |KK677 28
                    |KTJJT 220
                    |QQQJA 483""".stripMargin.split("\n").filter(_.nonEmpty).toSeq
  object Part1:
    val testResult = Solving.solve(testInput)
  object Part2:
    import DataDefs.Joker.given // get more specific givens!
    val testResult = Solving.solve(testInput)

//Testing.Part1.testResult // part 1: 6440
//Testing.Part2.testResult // part 2: 5905

object Main:
  val lines: Seq[String] = os.read.lines(os.pwd / "file")
  object Part1:
    val result = Solving.solve(lines)
  object Part2:
    import DataDefs.Joker.given // get more specific givens!
    val result = Solving.solve(lines)

@main
def main =
  println(Main.Part1.result) // part 1: 246795406
  println(Main.Part2.result) // part 2: 249356515