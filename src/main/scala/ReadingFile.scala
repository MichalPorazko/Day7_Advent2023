import scala.io.Source
import scala.util.Using

object ReadingFile {

  def toHand(input: String): Hand =
    val parts = input.split(" ").map(_.trim).toList
    val cards = parts.head
    val bid = parts(1).toInt
    Hand(cards, bid)

  def readFile(fileName: String): List[Hand] =
    Using(Source.fromFile(fileName)) { source =>
      source.getLines().foldLeft(List[Hand]().empty) { (list, string) =>
        toHand(string) :: list
      }
    }.getOrElse(sys.error("Cannot open input file."))


}
