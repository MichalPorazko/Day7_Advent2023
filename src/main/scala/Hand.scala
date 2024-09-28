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
    (countJ, sortedList) match
      case (5, _) => HandType.FiveKind
      case (4, _) => HandType.FiveKind
      case (3, 2 :: _) => HandType.FiveKind
      case (3, 1 :: _) => HandType.FourKind
      case (2, 3 :: _) => HandType.FiveKind
      case (2, 2 :: _) => HandType.FourKind
      case (2, 1 :: _) => HandType.ThreeKind
      case (1, 4 :: _) => HandType.FiveKind
      case (1, 3 :: _) => HandType.FourKind
      case (1, 2 :: 2 :: _) => HandType.FullHouse
      case (1, 2 :: 1 :: _) => HandType.ThreeKind
      case _ => HandType.OnePair

  type CompareLetter = (Char, Char) => Int

  val compareLetter: CompareLetter = (letter1: Char, letter2: Char) =>
    -digitStrengthPart1.indexOf(letter1).compare(digitStrengthPart1.indexOf(letter2))

  val compareLetterJ: CompareLetter = (letter1: Char, letter2: Char) =>
    -digitStrengthPart2.indexOf(letter1).compare(digitStrengthPart2.indexOf(letter2))

  given handOrdering(using compareLetter: CompareLetter): Ordering[Hand] = (a: Hand, b: Hand) => 
      val pairs = a.cards zip b.cards
      for((char1, char2) <- pairs){
        val cmp = compareLetter(char2, char1)
        if (cmp != 0){
          cmp
        }
      }
      0

  


}


