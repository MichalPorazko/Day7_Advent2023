import DataDefs.Card

class ExampleSolution extends munit.FunSuite {

  test("Understanding the groupMapReduce function") {
    val card = "KK677"

    // Step 1: Group by identity (Group cards by their face value)
    val groupByIdentity = card.groupBy(identity)
    println(s"Group by identity: $groupByIdentity")  // Outputs: Map(7 -> "77", K -> "KK", 6 -> "6")

    // Step 2: Transform values using mapValues to map each string to its length
    val usingMapValues = groupByIdentity.mapValues(string => string.length)
    println(s"Transformed using mapValues: $usingMapValues")  // Outputs: Map(7 -> 2, K -> 2, 6 -> 1)

    // Step 3: Apply reduction within mapValues (not needed in this example since map already gives the count)
    // If we were to reduce by another criteria (e.g., summing if values were numeric), it would be here

    // Step 4: Using reduce to further process if needed (omitted here as it doesn't apply directly)
    // For completeness, let's imagine we wanted to sum lengths for some reason
    val reducedResult = usingMapValues.values.reduce(_ + _)
    println(s"Reduced result: $reducedResult")  // Outputs the sum of the lengths

    case class Score(student: String, score: Double)

    val scores = List(
      Score("Alice", 88.0),
      Score("Bob", 92.5),
      Score("Alice", 95.0),
      Score("Bob", 85.0)
    )

    val sumAndCount = scores.groupMapReduce(s => s.student)(s => (s.score, 1)) {
      case ((sum1, count1), (sum2, count2)) => (sum1 + sum2, count1 + count2)
    }
    println(sumAndCount)

    val averagesByStudent = sumAndCount.map {
      case (student, (total, count)) => (student, total / count)
    }
    println(averagesByStudent)


  }



}
