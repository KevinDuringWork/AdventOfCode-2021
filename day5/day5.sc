import scala.io.Source
type Coordinate = (Int, Int)

val data = Source.fromFile("./input.txt").getLines.toList

def comma_to_coordinate(comma:String):Coordinate = {
    comma.split(",") match {case Array(x, y) => (x.toInt, y.toInt)}
}

val all_positions = data.foldLeft(List.empty[Coordinate])((acc, item) => {
    val result = item.split("->").map(_.trim) match {
        case Array(origin, destination) => {
            val ori = comma_to_coordinate(origin)
            val des = comma_to_coordinate(destination)

            // sort - considering less scenarios 
            val sorted = if (ori._1 <= des._1) (ori, des) else (des, ori) 

            sorted match {

                // vertical 
                case (ori, des) if (ori._1 == des._1) => {
                    (ori._2.min(des._2) to ori._2.max(des._2))
                    .toList.map((y) => (ori._1, y))
                }

                // horizontal 
                case (ori, des) if (ori._2 == des._2) => {
                    (ori._1.min(des._1) to ori._1.max(des._1))
                    .toList.map((x) => (x, ori._2))
                }

                // // diagonal at 45 deg - downward slop \ 
                case (ori, des) if (ori._2 < des._2) => {
                    val x = ori._1 to des._1
                    val y = ori._2 to des._2
                    x.zip(y).toList
                }

                // diagonal at 45 deg - upward slope / 
                case (ori, des) if (ori._2 > des._2)=> {
                    val x = ori._1 to des._1 
                    val y = ori._2 to des._2 by -1
                    x.zip(y).toList
                }
            }
        }
    }
    // println(s"${item} ${result}")
    acc ::: result   
})

val dup_map = all_positions.groupBy(identity).mapValues(_.length)


def dump(dup:Map[(Int, Int), Int]):Unit = {
    println("")
    for (y <- 0 to 9) {
        for (x <- 0 to 9) {
            dup.get((x,y)) match {
                case Some(v) => print(s"${v} ")
                case None => print(". ")
            }
        }
        println("")
    }
}

// dump(dup_map)

val dups = dup_map.foldLeft(0){case (a, (k, v)) => {
    if (v >= 2) a + 1 else a 
}}

println(dups)