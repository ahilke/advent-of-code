package d01historianhysteria

import java.io.File
import kotlin.math.abs

fun main() {
    val lines = File("src/main/kotlin/d01historianhysteria/input.txt").readLines()
    val (leftList, rightList) =
        lines.map {
            it.split(Regex("""\s+"""), limit = 2).let { (left, right) ->
                left to right
            }
        }.unzip()

    val orderedPairs = leftList.map { it.toInt() }.sorted() zip rightList.map { it.toInt() }.sorted()
    val sum = orderedPairs.fold(0) { sum, (left, right) -> sum + abs(left - right) }

    println("Part 01: $sum")

    val similarityScore =
        leftList.map { left -> left.toInt() * rightList.count { right -> left == right } }
            .sumOf { it }
    println("Part 02: $similarityScore")
}
