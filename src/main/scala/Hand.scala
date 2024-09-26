case class Hand(cards: String, bid: Int)

object Hand{

  private val digitStrengthPart1: Map[Char, Int] = Map(
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
  private val digitStrengthPart2: Map[Char, Int] = Map(
    'A' -> 13,
    'K' -> 12,
    'Q' -> 11,
    'T' -> 10,
    '9' -> 9,
    '8' -> 8,
    '7' -> 7,
    '6' -> 6,
    '5' -> 5,
    '4' -> 4,
    '3' -> 3,
    '2' -> 2,
    'J' -> 1
  )

  def charsCount(hand: Hand): Map[Char, Int] =
    hand.cards.groupBy(identity).map((c, s) => (c, s.length))

  private def defineType(list: List[Int]): HandType =
    //why is that that the 'list' in the cases is not recognised as the list parameter passed in the argument of the method?
    //I mean it's good but why???
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
    -digitStrengthPart1(letter1).compare(digitStrengthPart1(letter2))

  val compareLetterJ: CompareLetter = (letter1: Char, letter2: Char) =>
    -digitStrengthPart2(letter1).compare(digitStrengthPart2(letter2))

  /**
   val pairs = cards1 zip cards2
   for((char1, char2) <- pairs){
   val cmp = compareLetter(char1, char2)
   if (cmp != 0) {
   return cmp
   } else{
   compareCards(cards1.tail, cards2.tail)
   return 0
   }
   In Scala, using return is discouraged
   When you use return inside the for loop, the function exits on the first iteration.
   * */

  given handOrdering(using compareLetter: CompareLetter): Ordering[Hand] = new Ordering[Hand] {
    override def compare(a: Hand, b: Hand): Int =
      val pairs = a.cards zip b.cards
      for((char1, char2) <- pairs){
        val cmp = compareLetter(char2, char1)
        if (cmp != 0){
          return cmp
        }
      }
      0

  }


}


