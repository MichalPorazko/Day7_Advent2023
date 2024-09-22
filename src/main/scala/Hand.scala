case class Hand(cards: String, bid: Int)

object Hand{

  private val digitStrength: Map[Char, Int] = Map(
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

  def pairs(hand: Hand) =
    hand.cards.groupBy(identity).map((c, s) => (c, s.length)).values.toList.sorted.reverse

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
    defineType(pairs(hand))

  private def compareLetter(letter1: Char, letter2: Char) =
    -digitStrength(letter1).compare(digitStrength(letter2))


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


  given handOrdering: Ordering[Hand] = new Ordering[Hand] {
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


