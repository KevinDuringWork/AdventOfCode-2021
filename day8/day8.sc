import scala.io.Source

// lessons learned: 
// 1) how to memoize in scala?
// 2) excessive use of "break" is super ugly. 
// 3) minimize permutations # permutations with domain data? 

// format data 
// - sort the individual chars for easy reading 
// line : line(0) = Array(chars,..), line(1) = Array(chars..)
val data = Source.fromFile("./input.txt").getLines.toList
    .map(
        _.split("\\|")
        .map(_.trim.split(" ")
            .map(_.toList.sorted.mkString))
    )

// part one (1, 4, 7, 8) only care about the "output values"
val part1 = data.foldLeft(0)((acc, line) => {
    acc + line(1).foldLeft(0)((acc2, chars) => {
        if (Set(2, 4, 3, 7).contains(chars.length)) acc2 + 1 else acc2
    })
})

// part two: infer the rest of the results 
val constraints:Map[String, Int] = Map(
    "abcefg"  -> 0,
    "cf"      -> 1,
    "acdeg"   -> 2,
    "acdfg"   -> 3,
    "bcdf"    -> 4,
    "abdfg"   -> 5,
    "abdefg"  -> 6,
    "acf"     -> 7,
    "abcdefg" -> 8,
    "abcdfg"  -> 9
)

val ord  = List('a', 'b', 'c', 'd', 'e', 'f' ,'g')

def permuted_char(chars:String, perm:IndexedSeq[Int]): String = {
    chars.toList.map((c) => {
        ord(perm(ord.indexOf(c)))
    }).sorted.mkString
}

def filters(signals:List[String]):Map[Int, Set[Int]]  = {
    signals.foldLeft(Map.empty[Int, Set[Int]]){case (rule_map, signal) => {
        
        signal.length match {

            // rule (1) 
            case 2 => {
                val possible_rule_1 = signal.toList
                    .map(c => ord.indexOf(c))

                rule_map + (possible_rule_1(0) -> Set(2,5)) + (possible_rule_1(1) -> Set(2,5))
            }
            
            // rule (4)
            case 4 => {
                val possible_rule_4 = signal.toList
                    .map(c => ord.indexOf(c)).toSet
                    .diff(rule_map.map(_._1).toSet)
                    .toList

                rule_map + (possible_rule_4(0) -> Set(1,3)) + (possible_rule_4(1) -> Set(1,3))
            }

            // rule (7), affected by rule(1)
            case 3 => {
                val possible_rule_7 = signal.toList
                    .map(c => ord.indexOf(c)).toSet
                    .diff(rule_map.map(_._1).toSet)
            
                possible_rule_7.headOption match {
                    case Some(x) => rule_map + (x -> Set(0))
                    case None => rule_map 
                }
            }
            
            case _ => rule_map 
        }
    }}
}

def search(signals:Array[String], output:Array[String]): Int = {
    
    val rule_map = filters((signals ++ output).toSet.toList.
        sortBy((x:String) => x.size))

    val permutations = (0 until ord.size).permutations.filter((perm) => {
        // reduce permutation space w/ knowledge 
        rule_map.find({case (index:Int, possible:Set[Int]) => {

            // TODO: this can be more complex 
            possible.contains(perm(index)) == false 
        }}) match {
            case Some(x) => false 
            case None => true 
        }
    }).toList

    // println(s"${signals.toList} - ${output.toList}")
    // rule_map.foreach{println(_)}
    // permutations.foreach{println(_)}

    permutations.find(perm => {
        // find working permutation 
        signals.find(chars => {
            constraints.getOrElse(permuted_char(chars, perm) , -1) == -1
        }) match {
            case Some(x) => false
            case None => true
        }
    }) match {
        // apply permutation on results
        case Some(perm) => {
            output.foldLeft(List.empty[Int])(
                (acc, chars) => {
                    constraints(permuted_char(chars, perm)) :: acc 
                }).reverse.mkString.toInt
        }
        case None => -1 
    }
}

val part2 = data.foldLeft(List.empty[Int])((acc, entry) => {
    search(entry(0), entry(1)) :: acc 
}).sum

println(part1)
println(part2)
