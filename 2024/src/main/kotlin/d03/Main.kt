package d03

import java.io.File

fun main() {
    val reading = File("src/main/kotlin/d03/input.txt").readText()

    val matches =
        Regex(
            """mul\((\d+),(\d+)\)""",
        ).findAll(reading).sumOf { match ->
            val left = match.groups[1]
            val right = match.groups[2]

            if (left == null || right == null) {
                throw Exception("Unexpected regex match: ${match.groups[0]}")
            }

            left.value.toInt() * right.value.toInt()
        }

    println("Part 01: $matches") // 161 / 170068701
}
