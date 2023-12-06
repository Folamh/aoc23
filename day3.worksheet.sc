val inputLines = os.read.lines(os.pwd / "day3input.txt")

// 467..114..
// ...*......
// ..35..633.
// ......#...
// 617*......
// .....+.58.
// ..592.....
// ......755.
// ...$.*....
// .664.598..
val maxX = inputLines(0).length
val maxY = inputLines.length

def calcIndexesAroundIndex(x: Int, y: Int) =
  (for {
    x2 <- (x - 1) to (x + 1)
    y2 <- (y - 1) to (y + 1)
    if x2 >= 0 && x2 < maxX && y2 >= 0 && y2 < maxY
  } yield (x2, y2)).filterNot(_ == (x, y))

def removeAdjIndexes(indexes: IndexedSeq[(Int, Int)]) =
  indexes.filterNot { case (x, y) => indexes.contains((x - 1, y)) }

def findAdjNums(grid: Map[(Int, Int), Char], x: Int, y: Int) =
  var coursor = (x, y)
  // search left
  while (grid.contains(coursor) && grid(coursor).isDigit)
    coursor = (coursor._1 - 1, coursor._2)
  val left = coursor._1 + 1
  coursor = (x, y)
  // search right
  while (grid.contains(coursor) && grid(coursor).isDigit)
    coursor = (coursor._1 + 1, coursor._2)
  val right = coursor._1 - 1
  (left to right).map(grid(_, y)).mkString.toInt

val grid = inputLines.zipWithIndex.flatMap { case (line, y) =>
  line.zipWithIndex
    .map { case (char, x) =>
      (x, y) -> char
    }
    .filterNot(_._2 == '.')
}.toMap

val parts = grid.filterNot(_._2.isDigit)

// Part 1
parts.flatMap { case ((x: Int, y: Int), _) =>
  removeAdjIndexes(calcIndexesAroundIndex(x, y).filter(grid.contains(_))).map(i => findAdjNums(grid, i._1, i._2))
}.sum

// Part 2
parts.filter{case (_, c) => c == '*'}.map { case ((x: Int, y: Int), _) =>
  removeAdjIndexes(calcIndexesAroundIndex(x, y).filter(grid.contains(_))).map(i => findAdjNums(grid, i._1, i._2))
}.filter(_.length == 2).map(_.product).sum
