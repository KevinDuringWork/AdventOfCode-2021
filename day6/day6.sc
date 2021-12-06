import scala.io.Source
import scala.collection.mutable.Map
import scala.annotation.tailrec

val data = Source.fromFile("./input.txt").getLines.take(1)
    .mkString.split(",").toList.map((x) => BigInt(x))
    .groupBy(identity).mapValues((x) => BigInt(x.size))

@tailrec
def simulate(fishes:Map[BigInt, BigInt], curr:Int=0, limit:Int=10): Map[BigInt, BigInt] = {
    val update = Map[BigInt, BigInt]().withDefaultValue(BigInt(0))
    
    // this part was tricky, a simple notch in the loop got me 
    update(0) = fishes(1)
    update(1) = fishes(2)
    update(2) = fishes(3)
    update(3) = fishes(4)
    update(4) = fishes(5)
    update(5) = fishes(6) 
    update(6) = fishes(7) + fishes(0)
    update(7) = fishes(8)
    update(8) = fishes(0)

    if (curr + 1 != limit) simulate(update, curr + 1, limit) else update
}

val state = simulate(
    collection.mutable.Map(data.toSeq: _*).withDefaultValue(BigInt(0)), 0, 256
)

val count = state.foldLeft(BigInt(0)){case (acc, (k, v)) => acc + v}

println(count)