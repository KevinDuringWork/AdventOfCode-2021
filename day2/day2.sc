import scala.io.Source

// PART 1 
case class Vector(cardinal:String, magnitude: Int)

val vectors = Source.fromFile("./input.txt")
    .getLines.toList
    .map((line) => {
        val ls = line.split(" ")
        Vector(ls(0).toLowerCase, ls(1).toInt)
    })

val position = vectors.foldLeft((0,0))((acc, v) => {
    v.cardinal match {
        case "forward" =>   (acc._1 + v.magnitude, acc._2)
        case "up"      =>   (acc._1, acc._2 - v.magnitude)
        case "down"    =>   (acc._1, acc._2 + v.magnitude)
        case _         =>   acc 
    }
})

val result1 = position._1 * position._2

println("Part 1:")
println(position)
println(result1)

// Part 2 
val vector_aims = Source.fromFile("./input.txt")
    .getLines.toList
    .map((line) => {
        val ls = line.split(" ")
        Vector(ls(0).toLowerCase, ls(1).toInt)
    })

val position_aims = vector_aims.foldLeft((0,0,0))((acc, v) => {
    v.cardinal match {
        case "forward" =>   (acc._1 + v.magnitude, acc._2 + (acc._3 * v.magnitude), acc._3)
        case "up"      =>   (acc._1, acc._2, acc._3 - v.magnitude)
        case "down"    =>   (acc._1, acc._2, acc._3 + v.magnitude)
        case _         =>   acc 
    }
})

val result2 = position_aims._1 * position_aims._2

println("Part 2:")
println(position_aims)
println(result2)