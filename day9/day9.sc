import scala.io.Source
import scala.math 
import scala.collection.mutable

// recursion, we meet again!
// - is there a way to avoid blowing up the heap without a mutable

val data = Source.fromFile("./input.txt").getLines.toList.map(_.toList.map(_.asDigit))

def dump(data: List[List[Int]]) {
    for (y <- (0 until data.size)) {
        for (x <- (0 until data(y).size)) {
            print(s"${data(y)(x)} ")
        }
        println("")
    }
}

def lowest_local_pt(data: List[List[Int]]) : List[Tuple3[Int, Int, Int]]  = {
    val (h, w) = (data.size, data(0).size)

    (0 until w * h).foldLeft(List.empty[Tuple3[Int, Int ,Int]])((acc, coor) => {
        val (x, y) = (coor % w, coor / w)
        val p = data(y)(x)

        val local = List((-1, 0), (1, 0), (0, -1), (0, 1))
            .foldLeft(List.empty[Int])((acc, pt) => {
                val (y1, x1) = (y + pt._1, x + pt._2) 
                if ((x1 >= 0 && x1 < w) && (y1 >= 0 && y1 < h)) data(y1)(x1) :: acc else acc 
        })

        if (local.min > p) (y, x, p) :: acc else acc 
    }).reverse 
}

def basin(data: List[List[Int]], coor:Tuple2[Int, Int], visit:mutable.Set[Tuple2[Int, Int]]): List[Tuple2[Int, Int]] = {
    // println(s"basin: ${coor}=${data(coor._1)(coor._2)} | ${visit}")
    val (h, w) = (data.size , data(0).size)
    
    (coor._1, coor._2) :: List((-1, 0), (1, 0), (0, -1), (0, 1))
        .foldLeft(List.empty[Tuple2[Int, Int]])((acc, pt) => {
            val (y1, x1) = (coor._1 + pt._1, coor._2 + pt._2)
            if ((x1 >= 0 && x1 < w) && (y1 >= 0 && y1 < h)) {
                if (data(y1)(x1) < 9 && !visit((y1, x1))) {
                    visit.add((y1->x1))
                    basin(data, (y1, x1), visit) ::: acc
                } else acc
            } else acc
        })
}

// part 1
val part1 = lowest_local_pt(data).map(_._3 + 1).sum
val seeds = lowest_local_pt(data).map((x) => (x._1, x._2)) 

// part 2: using a mutable visited map.. 
val basins = seeds.foldLeft(List.empty[Int])((acc, seed) => {
    val mut_visit = mutable.Set[Tuple2[Int, Int]](seed)
    (basin(data, seed, mut_visit).toSet).size :: acc 
})

val part2 = basins.sorted.reverse.take(3).product

println(part1)
println(part2)