case class Hand(cards: String, bid: Int)

object Hand{

  private val digitStrengthPart1 = "23456789TJQKA"
  private val digitStrengthPart2 = "J23456789TQKA"

  def charsCount(hand: Hand): Map[Char, Int] =
    hand.cards.groupBy(identity).map((c, s) => (c, s.length))

  private def defineType(list: List[Int]): HandType =
    list match
      case 5 :: list => HandType.FiveKind
      case 4 :: list => HandType.FourKind
      case 3 :: 2 :: list => HandType.FullHouse
      case 3 :: _ :: list => HandType.ThreeKind
      case 2 :: 2 :: list => HandType.TwoPair
      case 2 :: list => HandType.OnePair
      case _ => HandType.HighCard

  def handType(hand: Hand): HandType =
    defineType(charsCount(hand).values.toList.sorted.reverse)

  def handTypeWithJ(hand: Hand): HandType =
    val pairs = charsCount(hand)
    val countJ = pairs.getOrElse('J', 0)
    val sortedList = (pairs - 'J').toList.sortBy((_, int) => -int).map((_, int) => int)
    defineType((countJ + sortedList.head) :: sortedList.tail)

  type CompareLetter = (Char, Char) => Int
  type DefineType = Hand => HandType

  val compareLetter: CompareLetter = (letter1: Char, letter2: Char) =>
    -digitStrengthPart1(letter1).compare(digitStrengthPart1(letter2))

  val compareLetterJ: CompareLetter = (letter1: Char, letter2: Char) =>
    -digitStrengthPart2(letter1).compare(digitStrengthPart2(letter2))

  given handOrdering(using compareLetter: CompareLetter, defineTypeOperation: DefineType): Ordering[Hand] =
    (hand1: Hand, hand2: Hand) => ???








}


