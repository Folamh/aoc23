val inputLines = os.read.lines(os.pwd / "day2input.txt")

case class Game(id: Int, rounds: List[Round])
case class Round(cubes: Map[Cube, Int])
trait Cube
case object Red extends Cube
case object Blue extends Cube
case object Green extends Cube
val gameRegex = """Game (\d+)""".r
val cubeRegex = """(\d+) (\w+)""".r
val games = inputLines.map { line =>
  val gameSplit = line.split(":")
  val roundSplit = gameSplit(1).split(";")
  val id = gameRegex.findFirstMatchIn(gameSplit(0)).get.group(1).toInt
  val rounds = roundSplit.map { round =>
    val cubes = round
      .split(",")
      .map { cube =>
        val cubeSplit = cubeRegex.findFirstMatchIn(cube).get
        val count = cubeSplit.group(1).toInt
        val color = cubeSplit.group(2) match {
          case "red"   => Red
          case "blue"  => Blue
          case "green" => Green
        }
        (color, count)
      }
      .toMap
    Round(cubes)
  }.toList
  Game(id, rounds)
}

// Part 1
val limit = Map(Red -> 12, Green -> 13, Blue -> 14)
games
  .filter(_.rounds.forall(_.cubes.forall { case (cube, count) =>
    count <= limit(cube)
  }))
  .map(_.id)
  .sum

// Part 2
games.map(_.rounds.map(_.cubes).fold(Map.empty) { case (left, right) =>
  left ++ right.map { case (k, v) =>
    k -> {
      left.get(k) match
        case None     => v
        case Some(v2) => if (v2 < v) v else v2
    }
  }
}).map(_.values.product).sum
