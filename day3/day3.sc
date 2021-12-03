import scala.io.Source

// Part 1
val signals = Source.fromFile("./input.txt")
    .getLines.toList

val counts = signals.foldLeft(List.fill(12)(0))(
        (acc, signal) => {
            acc zip signal.toList.map(_.asDigit) map {case (a,b) => a + b}
        }
    )

val gamma = Integer.parseInt(counts.map((count) => {
    if (count > signals.length / 2) 1 else 0
}).mkString, 2)

val epsilon = Integer.parseInt(counts.map((count) => {
    if (count > signals.length / 2) 0 else 1
}).mkString, 2)

println(gamma, epsilon, gamma * epsilon)

// Part 2
val signals2 = Source.fromFile("./input.txt")
    .getLines.toList

def major_minor(index: Int, signals:List[String], rating:String):Int = {
    val counts = signals.foldLeft(0)((acc, signal) => {
        acc + (signal(index).asDigit)
    })

    //println(s"counts, ${counts}/${signals.length}")

    rating match {
        case "oxygen" => {
            if (counts >= signals.length - counts) 1 else 0
        }
        case "co2" => {
            if (counts >= signals.length - counts) 0 else 1
        }
    }
}

def search(index:Int, signals:List[String], rating:String) : List[String] = {
    val bit = major_minor(index, signals, rating)
    val filtered = signals.filter((signal) => {
        signal.toList.map(_.asDigit).toList(index) == bit     
    })

    //println(s"search, filter:(index: ${index}, ${bit})")
    //println(s"> result: ${filtered}")

    val result = if (filtered.length > 1) {
        search(index + 1, filtered, rating) 
    } else { 
        filtered 
    }

    result
}

val oxygen = Integer.parseInt(search(0, signals2, "oxygen")(0), 2)

val co2 = Integer.parseInt(search(0, signals2, "co2")(0), 2)

println(oxygen, co2, oxygen*co2)