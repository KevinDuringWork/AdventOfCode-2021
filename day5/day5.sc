import scala.io.Source

type Coordinate = (Int, Int)

val data = Source.fromFile("./input.txt").getLines.toList

def csv2coord(comma:String):Coordinate = {
    comma.split(",") match {case Array(x, y) => (x.toInt, y.toInt)}
}

val all_positions = data.foldLeft(List.empty[Coordinate])((acc, item) => {
    
    val result = item.split("->").map(_.trim) match {
        
        case Array(origin, destination) => {
            val ori = csv2coord(origin)
            val des = csv2coord(destination)

            // sort - considering less scenarios (read from left to right)
            val sorted = if (ori._1 <= des._1) (ori, des) else (des, ori) 

            sorted match {

                // vertical 
                case (ori, des) if (ori._1 == des._1) => {
                    val dir = if (ori._2 < des._2) 1 else -1
                    (ori._2 to des._2 by dir).toList.map((y) => (ori._1, y))
                }
                    
                // horizontal 
                case (ori, des) if (ori._2 == des._2) => 
                    (ori._1 to des._1).toList.map((x) => (x, ori._2))
                

                // diagonal at 45 deg  
                case (ori, des) => {
                    val dir = if (ori._2 < des._2) 1 else -1
                    val xs = ori._1 to des._1
                    val ys = ori._2 to des._2 by dir 
                    xs.zip(ys).toList
                }
            }
        }
    }

    // println(s"${item} ${result}")
    acc ::: result   
})

val dup_map = all_positions.groupBy(identity).mapValues(_.length)

val dups = dup_map.foldLeft(0){case (a, (k, v)) => {
    if (v >= 2) a + 1 else a 
}}

def dump(dup:Map[(Int, Int), Int]):Unit = {
    println("")
    for (y <- 0 to 9) {
        for (x <- 0 to 9) {
            dup.get((x,y)) match {
                case Some(v) => print(s"${v}")
                case None => print(".")
            }
        }
        println("")
    }
}

// dump(dup_map)
println(dups)