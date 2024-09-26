import scala.io.Source
import scala.util.Using

object ReadingFile {

  def toHand(input: String): Hand =
    val parts = input.split(" ").map(_.trim).toList
    val cards = parts.head
    val bid = parts(1).toInt
    Hand(cards, bid)

  def readFile(fileName: String, camelCards: CamelCards): CamelCards =
    Using(Source.fromFile(fileName)) { source =>
      /**
       * Here's the problematic line:
       *
       * val cards = source.getLines().map(toHand).map(CamelCards.addToType(_, camelCards))
       * In this line:
       *
       * CamelCards.addToType(_, camelCards) returns a new CamelCards instance for each hand, but you're not updating camelCards with these new instances.
       * The camelCards instance passed into addToType remains unchanged throughout the mapping.
       * */
      source.getLines().foldLeft(camelCards){ (cards, string) =>
        CamelCards.addToType(toHand(string), cards, Hand.handType)
      }
    }.getOrElse(sys.error("Cannot open input file."))


}
