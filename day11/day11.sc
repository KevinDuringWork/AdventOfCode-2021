import scala.io.Source
import scala.annotation.tailrec

type Grid = Array[Array[Int]]
type Coordinate = (Int, Int)

val directions = List(
    (-1, -1),  (-1, 0),    (-1, 1), 
    (0, -1),   /*(0, 0)*/  (0, 1), 
    (1, -1),   (1, 0),     (1, 1))

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

@tailrec
def expansion(seeds:Set[Coordinate], next:Grid, vist:Grid): (Grid, Grid) = {
    val frontier = seeds.foldLeft(Set.empty[Coordinate])((acc, seed) => {            
        directions.foldLeft(Set.empty[Coordinate])((acc2, pt) => {
            val (y, x) = seed 
            val (y1, x1) = (y + pt._1, x + pt._2)

            // bounds 
            if ((y1 >= 0 && y1 < 10) && (x1 >= 0 && x1 < 10)) {

                // inclusion into new set 
                (next(y1)(x1), vist(y1)(x1)) match {
                    case (value, visited) if value >= 9 && visited == 0 => {
                        next(y1)(x1) = 0
                        vist(y1)(x1) = 1
                        acc2.union(Set((y1, x1)))
                    } 
                    case (value, visited) if value < 9 && visited == 0 =>{
                        next(y1)(x1) = next(y1)(x1) + 1
                        acc2
                    }
                    case _ => acc2 
                }
            } else acc2 
        }).union(acc)
    })

    if (frontier.size > 0) expansion(frontier, next, vist) else (next, vist)
}

def step(data:Grid): Grid = {
    val next_ = Array.ofDim[Int](10, 10)
    val vist_ = Array.ofDim[Int](10, 10)

    // increment by 1 
    for (y <- (0 until data.size)) {
        for (x <- (0 until data(y).size)) {
            next_(y)(x) = data(y)(x) + 1
        }
    }
    
    // flashes 
    val seeds = (0 until 10*10).foldLeft(Set.empty[Coordinate])((acc, coor) => {
        val (x, y) = (coor % 10, coor / 10)
        if (next_(y)(x) == 10) {
            next_(y)(x) = 0 
            vist_(y)(x) = 1
            acc.union(Set((y,x)))
         } 
         else acc 
    })

    // return the result of expansion of flashes 
    expansion(seeds, next_, vist_)._1
}

val part1 = (1 to 100).foldLeft((data:Grid, 0))((acc, iter) => {
    val update = step(acc._1)
    val count_flashes = update.foldLeft(0)((acc, y) => {
        acc + y.foldLeft(0)((acc2, v) => {
            if (v == 0) {acc2 + 1} else acc2
        })
    }) + acc._2 

    (update, count_flashes)
})

def part2(data:Grid, count:Int): (Grid, Int) = {
    val num_flashes = data.foldLeft(0)((acc, y) => {
        acc + y.foldLeft(0)((acc2, v) => {
            if (v == 0) {acc2 + 1} else acc2
        })
    })

    if (num_flashes != 100) part2(step(data), count+1) 
    else (data, count) 
}

println(part1._2)
println(part2(data, 0)._2)