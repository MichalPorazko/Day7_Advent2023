
class ExampleSolution extends munit.FunSuite {

  test("testing the apply method of object") {

   val cardMapPart1: Map[Char, Int] =
      Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2').reverse.zipWithIndex.toMap
   println(cardMapPart1)

    val cardMapPart2: Map[Char, Int] =
      Seq('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').reverse.zipWithIndex.toMap
    println(cardMapPart2)
  }


}
