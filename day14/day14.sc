import scala.io.Source
import collection.mutable 

val source = Source.fromFile("./input.txt").mkString.split("\n\n")
val template = source(0)
val polymers = source(1)
    .split("\n")
    .map(_.split("->").map(_.trim))
    .map{case Array(a, b) => (a, b)}
    .toList

// set up mapping  
// assuming unique mapping
val polymap:Map[String, Char] = polymers.groupBy(_._1)
    .mapValues(_(0)._2.charAt(0)) 

// set up initial distribution 
val template_map = template.sliding(2).toList
    .groupBy(identity).mapValues(_.size)

val polydis:Map[String, BigInt] = polymers.groupBy(_._1)
    .mapValues(x => {
        if (template_map.contains(x(0)._1)) BigInt(template_map(x(0)._1)) 
        else BigInt(0)
    })

// *************************************************** 
// NAIVE APPROACH 
// ***************************************************
// val naive = (1 to upto).foldLeft(template.toList)((poly, iter) => {    
//     val insert = poly.sliding(2).foldLeft(List.empty[Char])((acc, tup) => {
//         polymap(tup.mkString) :: acc 
//     }).reverse
//     val result = (poly zip insert).flatMap(v => List(v._1, v._2)) :+ poly(poly.size - 1)
//     result 
// })

// ***************************************************
// slightly smarter approach ... tally only distributions of XX 
// modifification from prior problem is that distributions change 
// NN -> NCN ~ +NC +CN -NN 
// ***************************************************
def dist(upto:Int) : Map[String,BigInt] = {
    (1 to upto).foldLeft(polydis)((poly, iter) => {
        val polydisM = mutable.Map(polymers.groupBy(_._1)
            .mapValues(x => BigInt(0)).toSeq: _*)
        
        for ((tup, ins) <- poly.filter{case (k,v) => v > 0}) {
            val left  = List(tup.charAt(0), polymap(tup)).mkString 
            val right = List(polymap(tup), tup.charAt(1)).mkString 

            //println(s"+${left} +${right} -${tup} x ${poly(tup)}")
            polydisM(tup)   = polydisM(tup)   - poly(tup)
            polydisM(left)  = polydisM(left)  + poly(tup)
            polydisM(right) = polydisM(right) + poly(tup)
        }

        // update 
        polymers.groupBy(_._1).map{case (k,v) => (k, poly(k) + polydisM(k))}
    })
}

def count(dist:Map[String, BigInt], letter:Char):BigInt = {
    val result = dist.foldLeft((BigInt(0), BigInt(0))){case (acc,v) => {
        val poly_set = v._1.toSet
        val value = dist(v._1)
        if(poly_set.contains(letter)) {
            if (poly_set.size == 1) {
                (acc._1 + value, acc._2)
            } else {
                (acc._1, acc._2 + value)
            }
        } else acc
    }}

    result._1 + (result._2 + 1) / 2
}

def frequency_map(dist:Map[String, BigInt]) : List[(Char, BigInt)] = {
    polymap.map(_._2).toSet.foldLeft(List.empty[(Char, BigInt)])((acc, char) => {
        (char, count(dist, char)) :: acc
    }).sortBy(_._2)
}

val p1map = frequency_map(dist(10))
val part1 = p1map.takeRight(1)(0)._2 - p1map.take(1)(0)._2
println(part1)

val p2map = frequency_map(dist(40))
val part2 = p2map.takeRight(1)(0)._2 - p2map.take(1)(0)._2
println(part2)