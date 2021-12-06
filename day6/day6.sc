import scala.io.Source
import scala.collection.mutable.Map
import scala.annotation.tailrec

val data = Source.fromFile("./input.txt").getLines.take(1)
    .mkString.split(",").toList.map(_.toInt)
    .groupBy(identity).mapValues((x) => BigInt(x.size))

@tailrec
def simulate(fishes:Map[Int, BigInt], curr:Int=0, limit:Int=10): Map[Int, BigInt] = {
    val update = fishes(0)

    for (i <- 0 to 7) {fishes(i) = fishes(i + 1)}
    fishes(8) = update
    fishes(6) = fishes(6) + update

    if (curr + 1 != limit) simulate(fishes, curr + 1, limit) else fishes
}

val part1 = simulate(collection.mutable.Map(data.toSeq: _*).withDefaultValue(BigInt(0)), 0, 80)
    .foldLeft(BigInt(0)){case (acc, (k, v)) => acc + v}

val part2 = simulate(collection.mutable.Map(data.toSeq: _*).withDefaultValue(BigInt(0)), 0, 256)
    .foldLeft(BigInt(0)){case (acc, (k, v)) => acc + v}

println(part1)
println(part2)