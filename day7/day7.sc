import scala.io.Source

val data = Source.fromFile("./input.txt").getLines.take(1)
    .mkString.split(",").toList.map(_.toInt)

// brute force search 
def lookup(cost: (Int, Int) => Int): Map[Int,Int] = {
    (data.min to data.max).foldLeft(List.empty[(Int, Int)])((acc, align) => {
        val total_cost = data.foldLeft(0)((current_cost, pos) => {
            current_cost + cost(align, pos)
        })
        acc :+ (align, total_cost)
    })
    .groupBy(_._1)
    .map { case (k,v) => (v.map(_._2).min, k)}
}

val part1 = lookup((align, pos) => (align - pos).abs)
val part2 = lookup((align, pos) => (0 to (align - pos).abs).sum)

val min_fuel1 = part1.map(_._1).min 
println(s"${min_fuel1} @ position ${part1(min_fuel1)}")

val min_fuel2 = part2.map(_._1).min 
println(s"${min_fuel2} @ position ${part2(min_fuel2)}")
    


