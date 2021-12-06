import scala.io.Source
import scala.collection.mutable.Map
import scala.annotation.tailrec

val data = Source.fromFile("./input.txt").getLines.take(1)
    .mkString.split(",").toList.map(_.toInt)
    .groupBy(identity).mapValues((x) => BigInt(x.size))

@tailrec
def simulate(fishes:Map[Int, BigInt], curr:Int=0, limit:Int=10): Map[Int, BigInt] = {
    val update = Map[Int, BigInt]().withDefaultValue(BigInt(0))
    
    // time decrement 
    for (i <- 0 to 8) {
        update(i) = fishes((i + 1) % 9)
    }

    // reset timer 
    update(6) = update(6) + fishes(0)

    if (curr + 1 != limit) simulate(update, curr + 1, limit) else update
}

val state = simulate(collection.mutable.Map(data.toSeq: _*).withDefaultValue(BigInt(0)), 0, 256)
val count = state.foldLeft(BigInt(0)){case (acc, (k, v)) => acc + v}
println(count)