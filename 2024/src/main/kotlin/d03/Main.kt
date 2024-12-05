package d03

import java.io.File

fun main() {
    val reading = File("src/main/kotlin/d03/input.txt").readText()

    val matches =
        Regex("""mul\((\d+),(\d+)\)""")
            .findAll(reading).sumOf { match ->
                val (left, right) = match.destructured
                left.toInt() * right.toInt()
            }

    println("Part 01: $matches") // 161 / 170068701
}
