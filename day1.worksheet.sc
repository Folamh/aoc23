import scala.util.Sorting

val inputLines = os.read.lines(os.pwd / "day1input.txt")

// Part 1
inputLines
  .map(line =>
    s"${line.find(_.isDigit).get}${line.findLast(_.isDigit).get}".toInt
  )
  .sum

// Part 2
val nums = Map(
  "one" -> 1,
  "two" -> 2,
  "three" -> 3,
  "four" -> 4,
  "five" -> 5,
  "six" -> 6,
  "seven" -> 7,
  "eight" -> 8,
  "nine" -> 9
)
val findNums = (line: String) => {
  val firstWords = nums.keys
    .map(num => (line.indexOf(num), nums(num)))
    .filter(_._1 > -1)
    .toList
  val lastWords = nums.keys
    .map(num => (line.lastIndexOf(num), nums(num)))
    .filter(_._1 > -1)
    .toList
  val first = {
    val f = line.indexWhere(_.isDigit)
    (f, line(f).asDigit)
  }
  val last = {
    val f = line.lastIndexWhere(_.isDigit)
    (f, line(f).asDigit)
  }
  val imp = (firstWords ++ lastWords :+ first :+ last).sorted
  s"${imp.head._2}${imp.last._2}".toInt
}

inputLines.map(findNums).sum
