import scala.io.Source

val sample = Source.fromFile("./sample.txt").getLines.toList.map(_.toInt)
val lines = Source.fromFile("./input.txt").getLines.toList.map(_.toInt)

// part 1 
val result = lines.sliding(2).foldLeft(0)(
  (acc, y) => {if (y(1) > y(0)) acc + 1 else acc }
)
println(result)

val result2 = lines.sliding(3).map(_.sum).sliding(2).foldLeft(0)(
  (acc, y) => {if (y(1) > y(0)) acc + 1 else acc }
)
println(result2)

