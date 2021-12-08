import scala.io.Source
import scala.util.control.Breaks._

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

def search(signals:Array[String], output:Array[String]): Int = {
    var search_result:Int = -1
    
    breakable {
        for (perm <- (0 to 6).permutations) {

            // trim search... optimization 
            var valid = true 
            breakable {
                for (chars <- signals) {
                    val p_char  = permuted_char(chars, perm) 
                    val p_value = constraints.getOrElse(p_char, -1)
                    if (p_value == -1) {
                        valid = false
                        break 
                    }
                }
            }
            
            // process output
            if (valid) {
                val result = output.foldLeft(List.empty[Int])((acc, chars) => { 
                    constraints(permuted_char(chars, perm)) :: acc 
                })
            
                search_result = result.reverse.mkString.toInt
                break
            }   
        }
    }

    search_result
}

val part2 = data.foldLeft(List.empty[Int])((acc, entry) => {
    search(entry(0), entry(1)) :: acc 
}).reverse.sum

println(part1)
println(part2)
