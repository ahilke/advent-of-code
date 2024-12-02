package d01historianhysteria

import java.io.File
import kotlin.math.abs

fun main() {
    val (leftList, rightList) =
        File("src/main/kotlin/d01historianhysteria/input.txt").readLines()
            .map { line -> line.split(Regex("""\s+"""), limit = 2) }
            .map { (left, right) -> left.toInt() to right.toInt() }
            .unzip()

    val sum =
        leftList
            .sorted()
            .zip(rightList.sorted())
            .sumOf { (left, right) -> abs(left - right) }

    println("Part 01: $sum") // 3714264

    val similarityScore =
        leftList
            .sumOf { left -> left * rightList.count { right -> right == left } }

    println("Part 02: $similarityScore") // 18805872
}
