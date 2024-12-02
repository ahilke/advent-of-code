package d02rednosedreports

import java.io.File

fun main() {
    val readings =
        File("src/main/kotlin/d02rednosedreports/input.txt").readLines()
            .map { line -> line.split(Regex("""\s+""")) }
            .map { values -> values.map { value -> value.toInt() } }

    val strictSafeReadings = readings.count { isSafe(it) }

    println("Part 01: $strictSafeReadings") // 2 / 486

    val safeLevels =
        readings
            .map { levels -> levels.mapIndexed { index, level -> levels.toMutableList().apply { removeAt(index) }.toList() } }
            .count { levelVariations -> levelVariations.any { isSafe(it) } }

    println("Part 02: $safeLevels") // 4 / 540
}

fun isSafe(levels: List<Int>): Boolean {
    return isSafelyIncreasing(levels) || isSafelyDecreasing(levels)
}

fun isSafelyIncreasing(levels: List<Int>): Boolean {
    return levels.zipWithNext().all { (lower, higher) -> higher - lower >= 1 && higher - lower <= 3 }
}

fun isSafelyDecreasing(levels: List<Int>): Boolean {
    return levels.zipWithNext().all { (higher, lower) -> higher - lower >= 1 && higher - lower <= 3 }
}
