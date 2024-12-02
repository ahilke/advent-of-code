package d02rednosedreports

import java.io.File

fun main() {
    val sum =
        File("src/main/kotlin/d02rednosedreports/input.txt").readLines()
            .map { line -> line.split(Regex("""\s+""")) }
            .map { values -> values.map { value -> value.toInt() } }
            .count { levels -> isSafelyIncreasing(levels) || isSafelyDecreasing(levels) }

    println("Part 01: $sum") // 486
}

fun isSafelyIncreasing(levels: List<Int>): Boolean {
    return levels.zipWithNext().all { (lower, higher) -> higher - lower >= 1 && higher - lower <= 3 }
}

fun isSafelyDecreasing(levels: List<Int>): Boolean {
    return levels.zipWithNext().all { (higher, lower) -> higher - lower >= 1 && higher - lower <= 3 }
}
