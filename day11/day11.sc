import scala.io.Source
import scala.collection.mutable

type Grid = Array[Array[Int]]

val data = Source.fromFile("./input.txt").getLines
    .toArray.map(_.toArray.map(_.asDigit))

def dump(data: Grid) {
    for (y <- (0 until data.size)) {
        for (x <- (0 until data(y).size)) {
            val p = data(y)(x)
            if (p == 10) print(s"\033[4m10\033[0m ") else print(s"${p} ")
        }
        println("")
    }
}

def iteration(data:Grid): Grid = {
    val next_ = Array.ofDim[Int](10, 10)
    val vist_ = Array.ofDim[Int](10, 10)
    val directions = List(
        (-1, -1),   (-1, 0),      (-1, 1), 
        (0, -1),    /*(0, 0),*/   (0, 1), 
        (1, -1),    (1, 0),       (1, 1))

    // setup 
    for (y <- (0 until data.size)) {
        for (x <- (0 until data(y).size)) {
            next_(y)(x) = data(y)(x) + 1
        }
    }
    
    // initial flashes 
    var seeds = (0 until 10*10).foldLeft(mutable.Set.empty[(Int, Int)])((acc, coor) => {
        val (x, y) = (coor % 10, coor / 10)
        if (next_(y)(x) == 10) {
            next_(y)(x) = 0 
            vist_(y)(x) = 1
            acc.union(Set((y,x)))
         } 
         else acc 
    })

    while (seeds.size > 0) {
        seeds = seeds.foldLeft(mutable.Set.empty[(Int, Int)])((acc, seed) => {            
            directions.foldLeft(mutable.Set.empty[(Int, Int)])((acc2, pt) => {
                val (y, x) = seed 
                val (y1, x1) = (y + pt._1, x + pt._2)

                // bounds 
                if ((y1 >= 0 && y1 < 10) && (x1 >= 0 && x1 < 10)) {

                    // inclusion into new set 
                    (next_(y1)(x1), vist_(y1)(x1)) match {
                        case (value, visited) if value >= 9 && visited == 0 => {
                            next_(y1)(x1) = 0
                            vist_(y1)(x1) = 1
                            acc2.union(Set((y1, x1)))
                        } 
                        case (value, visited) if value < 9 && visited == 0 =>{
                            next_(y1)(x1) = next_(y1)(x1) + 1
                            acc2
                        }
                        case _ => acc2 
                    }
                } else acc2 
            }).union(acc)
        })
    }

    next_
}

val part1 = (1 to 100).foldLeft((data:Grid, 0))((acc, iter) => {
    val update = iteration(acc._1)
    val count_flashes = update.foldLeft(0)((acc, y) => {
        acc + y.foldLeft(0)((acc2, v) => {
            if (v == 0) {acc2 + 1} else acc2
        })
    }) + acc._2 

    (update, count_flashes)
})

val part2 = (1 to 195).foldLeft((data:Grid, 0))((acc, iter) => {
    val update = iteration(acc._1)
    val count_flashes = update.foldLeft(0)((acc, y) => {
        acc + y.foldLeft(0)((acc2, v) => {
            if (v == 0) {acc2 + 1} else acc2
        })
    }) + acc._2 

    (update, count_flashes)
})

def count_flashes(data:Grid) : Int = {
    data.foldLeft(0)((acc, y) => {
        acc + y.foldLeft(0)((acc2, v) => {
            if (v == 0) {acc2 + 1} else acc2
        })
    })
}
def part2(data:Grid, count:Int): (Grid, Int) = {
    if (count_flashes(data) != 100) part2(iteration(data), count+1) else (data, count) 
}

println(part1._2)
println(part2(data, 0)._2)