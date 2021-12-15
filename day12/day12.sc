import scala.io.Source

type Location = String 
type Path = List[Location] 

val data_map = Source.fromFile("./input.txt")
    .getLines.map(_.split("-").toList)
    .map(line => { 
        line match {
            // Bidirected graph..
            case List(a, b) => List((a, b), (b, a))
        }
    }).flatten.toList.groupBy(_._1)
    .mapValues(_.map(_._2))

def traverse(current:Location, paths_taken:Path, visited:Set[Location], penalty:Int):Set[Path] = {
    
    // println(s"${paths_taken.reverse}:${current}:${visited}")
    data_map(current).foldLeft(Set.empty[Path])(
    (acc, location) => {
        location match {

            case "end"   => acc.union(Set((location :: paths_taken).reverse))
            case "start" => acc.union(Set.empty[Path])

            // large cave - no move penalty at all 
            case large_cave 
                if large_cave != large_cave.toLowerCase => 
        
                    acc.union(traverse(
                        large_cave,
                        large_cave :: paths_taken,
                        visited,
                        penalty
                    ))
            
            // small cave - no move penalty incurred, visited increased
            case small_cave 
                if (small_cave == small_cave.toLowerCase && 
                !visited.contains(small_cave)) => 
                
                    acc.union(traverse(
                        small_cave, 
                        small_cave :: paths_taken, 
                        visited.union(Set(small_cave)),
                        penalty
                    ))
            
            // small cave - move penalty incurred 
            case small_cave_penalty 
                if (small_cave_penalty == small_cave_penalty.toLowerCase && 
                    visited.contains(small_cave_penalty) && 
                    penalty > 0) => 

                    acc.union(traverse(
                        small_cave_penalty, 
                        small_cave_penalty :: paths_taken, 
                        visited.union(Set(small_cave_penalty)),
                        penalty - 1
                    ))
            
            // dead end 
            case _ => acc.union(Set.empty[Path])
        }
    })
}

val part1 = traverse("start", List("start"), Set("start"), 0)
val part2 = traverse("start", List("start"), Set("start"), 1)

println(part1.size)
println(part2.size)