import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.util.control._

case class BingoSquare(value:Int, checked:Boolean)

val data   = Source.fromFile("./input.txt").getLines
val inpu   = data.take(1).mkString.trim.split(',').map(_.toInt).toList
var boards = new ListBuffer[Array[Array[BingoSquare]]]()

// setup 
for (line <- data) {    
    if (line.length == 0){
        boards += data.take(5).toArray
            .map(
                _.trim.split("\\s+")
                .map(_.trim.toInt)
                .map((v) => {
                    BingoSquare(v, false)
                })
            )
    }
}

// assignment 
def assign(board:Array[Array[BingoSquare]], value:Int) : Unit  = {
    for (i <- 0 until 5) {
        for (j <- 0 until 5) {
            if (board(i)(j).value == value) {
                board(i)(j) = BingoSquare(value, true)
            }
        }
    }
}

// evaluate win 
def evaluate(board:Array[Array[BingoSquare]]):Option[Array[Int]] = {
    val row_inner_loop = new Breaks;
    val column_inner_loop = new Breaks;

    // row evaluation 
    for (i <- 0 until 5) {
        row_inner_loop.breakable {
            val row = Array.fill(5)(0)
            for (j <- 0 until 5) {
                if (board(i)(j).checked) {
                    //println(s"${row.toList} (${j}) => ${board(i)(j).value}")
                    row(j) = board(i)(j).value
                } else {
                    row_inner_loop.break
                }
            }
            return Option(row)
        }
    }

    // column evaluation 
    for (j <- 0 until 5) {
        column_inner_loop.breakable {
            val column = Array.fill(5)(0)
            for (i <- 0 until 5) {
                if(board(i)(j).checked) {
                    //println(s"${column.toList} (${i}) => ${board(i)(j).value}")
                    column(i) = board(i)(j).value 
                } else {
                    column_inner_loop.break 
                }
            }
            return Option(column)
        }
    }

    None
}

// personal sanity check 
def dump(board:Array[Array[BingoSquare]]) : Unit = {
    for (i <- 0 until 5) {
        print("[")
        for (j <- 0 until 5) {
            val checked = if (board(i)(j).checked) "(x)" else ""
            print(s"${board(i)(j).value}${checked}\t")
        }
        println("]")
    }
    println("")
}

// "score" of the board 
def calculate_score(winning_board:Array[Array[BingoSquare]]) : Int = {
    winning_board.foldLeft(0)((total:Int, col:Array[BingoSquare]) => {
        total + col.foldLeft(0)((sub, square:BingoSquare) => {
            if (square.checked) sub else sub + square.value
        })
    })
}

def part1() : Unit = {
    val main_loop = new Breaks;
    main_loop.breakable {
        for (i <- inpu) {
            //println(s"adding ${i}\n")

            for (b <- 0 until boards.length) {
                assign(boards(b), i)
                evaluate(boards(b)) match {
                    case Some(answer) => {
                        val final_score = calculate_score(boards(b)) * i
                        println(s"BINGO: board:${b}: ${answer.toList}, == ${final_score}")
                        main_loop.break 
                    }
                    case None => ()
                }
                //dump(boards(b))
            }
        }
    }
}

def part2() : Unit = {
    val outer_loop = new Breaks;
    val inner_loop = new Breaks;
    val board_set = (0 until boards.length).toSet
    var winning_boards = Set[Int]() 

    outer_loop.breakable {
        for (i <- inpu) {
            //println(s"adding ${i}\n")

            for (b <- board_set.diff(winning_boards)) {
                assign(boards(b), i)
            }


            for (b <- board_set.diff(winning_boards)) {
                evaluate(boards(b)) match {
                    case Some(answer) => {
                        // println(s"BINGO: board:${b}: ${answer.toList}")
                        winning_boards.add(b)

                        if (board_set.diff(winning_boards).size == 0) {
                            
                            val final_score = calculate_score(boards(b)) * i
                            println(s"Result (letting opponent win) board:${b}, ${i} == ${final_score}")
                            
                            outer_loop.break
                        }
                    }
                    case None => ()
                }

                // dump(boards(b))
            }
            
        }
    }
}


part1()
part2()
