// not sure if right - took long to finish part2 

import scala.io.Source
import collection.mutable 
import scala.util.control.Breaks._

type Grid = Array[Array[Int]]
type Location = Tuple2[Int, Int]
type Priority = (Int, Location)

val data = Source.fromFile("./input.txt").getLines
    .toArray.map(_.toArray.map(_.asDigit))

def dump(data: Grid, visited:mutable.Map[Location, Set[Location]]) {
    for (y <- (0 until data.size)) {
        for (x <- (0 until data(y).size)) {
            val p = data(y)(x)
            val v = if(visited.contains((y,x))) visited((y,x)).size else 0
            print(s"${p}(${v}) \t")
        }
        println("")
    }
}

def cache_dump(data:Grid, cache: mutable.Map[Location, (Int, Location)], 
    visited:mutable.Map[Location, Set[Location]]): Unit = {

    val h = data.size
    val w = data(0).size
    val m = Array.ofDim[Int](h, w)

    for (c <- cache) {
        for (y <- (0 until h)) {
            for (x <- (0 until w)) {
                m(c._1._1)(c._1._2) = c._2._1 
            }
        }
    }

    dump(m, visited)
}

def super_map(data:Grid, multiple:Int):Grid = {
    val h = data.size
    val w = data(0).size
    val smap = Array.ofDim[Int](h * multiple, w * multiple)

    for (y1 <- 0 until multiple) {
        for (x1 <- 0 until multiple) {
            for (y <- (0 until h)) {
                for (x <- (0 until w)) {
                    val value = (data(y)(x) + y1 + x1)
                    val set = if (value % 9 == 0) 9 else value % 9
                    // val set  = if (value > 9) value - 9 else value

                    if (set > 9) println("error")
                    smap(y1*h + y)(x1*w + x) = set 
                }
            }
        }
    }
    smap
}

def neighbors(data:Grid, loc:Location) : Set[Location] = {
    val h = data.size 
    val w = data(0).size

    // possible directions 
    val (u, d, r, l) = (
        (loc._1 - 1, loc._2),
        (loc._1 + 1, loc._2), 
        (loc._1, loc._2 + 1),
        (loc._1, loc._2 - 1),
    )

    // bounded conditions 
    Set(u, d, r, l).filter(dir => 
        (dir._1 < h) && (dir._1 >= 0 ) && 
        (dir._2 < w) && (dir._2 >= 0 ))

}

def risk(data:Grid, loc:Location, visited:mutable.Map[Location, Set[Location]], 
    cache:mutable.Map[Location, (Int, Location)]) : Set[Priority] = {

    // empty || not visited enough  
    val valid_directions = neighbors(data, loc).filter(dir => {
        !visited.contains(dir) || neighbors(data, dir).size > visited(dir).size
    })
    
    // update distances 
    val priority = mutable.Set.empty[Priority]
    
    // update occurred 
    for (d <- valid_directions) {
        val acc_penalty = data(d._1)(d._2) + cache(loc)._1 
        val empty_cond  = !cache.contains(d)
        val update_cond = cache.contains(d) && cache(d)._1 > acc_penalty

        if (empty_cond || update_cond) {
            cache(d) = (acc_penalty, loc)
            
            // neighbors may need updating, ignore origin (loc)
            for ( n <- neighbors(data, d).diff(Set(loc))) {
                if (visited.contains(n)) {visited(n) = visited(n).diff(Set(d))}
            }
            
            // location d visited by loc
            if (visited.contains(d)) visited(d) = visited(d).union(Set(loc)) 
                else visited += (d -> Set(loc))

            priority.add((acc_penalty, d))
        }
    }

    priority.toSet
}

def evaluate(data:Grid) : Int = {

    def PriorityOrder(d:Priority) = d._1

    // setup for top and bottom elements 
    val top_left = (0,0)
    val bottom_right = (data.size - 1, data(0).size - 1)

    // setup state 
    val cache    = mutable.Map(top_left -> (0, top_left))
    val visited  = mutable.Map(top_left -> Set((1,0), (0,1)))
    val workload = mutable.PriorityQueue.empty[Priority](Ordering.by(PriorityOrder)).reverse

    workload.enqueue((1, top_left)) 

    while (!workload.isEmpty) {
        val (priority, loc) = workload.dequeue()
        val res = risk(data, loc, visited, cache)
        res.foreach(x=>{workload.enqueue(x)})

        println(workload)
        cache_dump(data, cache, visited)

        if (cache.contains(bottom_right)) return cache(bottom_right)._1
    }

    // return bottom element
    cache(bottom_right)._1
}

val part1 = evaluate(data)
println(part1)

val part2 = evaluate(super_map(data, 5))
println(part2)