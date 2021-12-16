import scala.io.Source

type Grid = Array[Array[Int]]
type SliceRange = Tuple2[Int, Int]

// Setup inputs
val source = Source.fromFile("./input.txt").mkString.split("\n\n")
val points = source(0).split("\n")
    .map(_.trim.split(","))
    .map(pt => 
        pt match {
            case Array(a, b) => (a.toInt, b.toInt)
        }
    )
    .toList

val max_y = points.map(_._2).max + 1
val max_x = points.map(_._1).max + 1
val data = Array.ofDim[Int](max_y, max_x)
val inst = source(1).split("\n")

for (pt <- points) {
    data(pt._2)(pt._1) = 1
}

def dump(data: Grid) {
    for (y <- (0 until data.size)) {
        for (x <- (0 until data(y).size)) {
            val p = if (data(y)(x) >= 1) "#" else "."
            // val p = data(y)(x)
            print(s"${p} ")
        }
        println("")
    }
}

// Grid -> Slice 
def slice(data: Grid, yR:SliceRange, xR: SliceRange) : Grid = 
    data.slice(yR._1, yR._2).map(_.slice(xR._1, xR._2))

// Grid -> Reflection 
def reflect(data: Grid, direction: String) : Grid  = {
    val ndata = Array.ofDim[Int](data.size, data(0).size)
    for (y <- (0 until ndata.size)) {
        for (x <- (0 until ndata(0).size)) {
            direction match {
                case "vertical"   => ndata(y)(x) = data(y)((data(0).size - 1 - x))
                case "horizontal" => ndata(y)(x) = data((data.size - 1 - y))(x)
            }
        }
    }
    ndata
}

// Grid -> Merge folded layer against the (larger) base layer
def merge(base:Grid, folded:Grid, direction:String):Grid = {

    val res = Array.ofDim[Int](base.size, base(0).size)

    // merging base and folded w/ an offset 
    direction match {

        case "left" => {
            val (yd, xd) = (0, base(0).size - folded(0).size)
            for (y <- (0 until res.size)) {
                for (x <- (0 until res(0).size)) {
                    if (x - xd < 0) res(y)(x) = base(y)(x) 
                    else res(y)(x) = base(y)(x).max(folded(y)(x - xd))
                }
            }
            res
        }
            
        case "up" => {
            val (yd, xd) =  (base.size - folded.size, 0)
            for (y <- (0 until res.size)) {
                for (x <- (0 until res(0).size)) {
                    if (y - yd < 0) res(y)(x) = base(y)(x) 
                    else res(y)(x) = base(y)(x).max(folded(y - yd)(x))
                }
            }
            res 
        }
    }
}

def rH(data:Grid):Grid = reflect(data, "horizontal")
def rV(data:Grid):Grid = reflect(data, "vertical")

// Grid -> Fold 
def fold(data:Grid, direction:String, value:Int) : Grid = {
    
    // define two layers (Grid): "base" and "fold"
    // merged folded onto base shifted accordingly 

    val h = data.size 
    val w = data(0).size 

    direction match {
         case "left" => {
            val base = slice(data, (0,h), (0,value))
            val fold = rV(slice(data, (0,h), (value + 1, w)))
            val merged = merge(base, fold, direction)
            merged
            
         }
         case "up" => {
            val base = slice(data, (0, value), (0, w))
            val fold = rH(slice(data, (value+1, h), (0, w)))
            val merged = merge(base, fold, direction)
            merged
         }
    }
}

val part1 = inst.take(1).foldLeft(data){case (grid, i) => {
    i.split(" ").takeRight(1)(0).split("=") match {
        case Array("y", v) => fold(grid, "up", v.toInt)
        case Array("x", v) => fold(grid, "left", v.toInt)
    }
}}

val part2 = inst.foldLeft(data){case (grid, i) => {
    i.split(" ").takeRight(1)(0).split("=") match {
        case Array("y", v) => fold(grid, "up", v.toInt)
        case Array("x", v) => fold(grid, "left", v.toInt)
    }
}}

println(part1.flatten.sum)
dump(part2)