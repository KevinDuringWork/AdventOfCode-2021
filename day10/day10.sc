import scala.io.Source

val open_symbols  = List('(', '{', '[', '<')
val close_symbols = List(')', '}', ']', '>')
val data = Source.fromFile("./input.txt").getLines.toList

type ParsedLine = Tuple2[Char, Array[Char]]

def parse_line(line:String):ParsedLine = {
    line.toList.foldLeft(('$', Array.empty[Char])){ 
        case ((expect, stack), char) => {
            // println(s"${char} - ${expect} - ${stack.toList}")
            (expect, char) match {
                
                // error propagation ... 
                case ('x', _) => ('x', stack)

                // open detected, add to stack 
                case (_,  sym) if open_symbols.indexOf(sym) != -1 => 
                    val closed_sym = close_symbols(open_symbols.indexOf(sym))
                    (closed_sym, stack :+ closed_sym)
                
                // close detected, attempt to pop from stack or revert to completion
                case (closed, sym) if closed == sym => {
                    if (stack.length > 1) (stack.dropRight(1).last, stack.dropRight(1)) 
                    else ('$', Array.empty[Char])
                }

                // error occured
                case _ => {
                    ('x', Array(expect, char))
                }
            }
        }
    }
}

val processed = data.foldLeft(List.empty[ParsedLine])((acc, line) => {
    parse_line(line) :: acc 
}).reverse

val part1 = processed.filter(_._1 == 'x').foldLeft(0)((acc, error) => {
    acc + (error._2.toList.last match {
        case ')' => 3
        case ']' => 57
        case '}' => 1197
        case '>' => 25137
    })
})

val part2_data = processed.filter(_._1 != 'x').foldLeft(List.empty[BigInt])((acc, error) => {
    error._2.toList.reverse.foldLeft(BigInt(0))((acc_1, char) => {
        (acc_1 * 5) + (char match {
            case ')' => BigInt(1)
            case ']' => BigInt(2)
            case '}' => BigInt(3)
            case '>' => BigInt(4)
        })
    }) :: acc 
}).sorted

val part2 = part2_data(part2_data.size / 2)


/// 126445911 -- too low 
println(part1)
println(part2)
