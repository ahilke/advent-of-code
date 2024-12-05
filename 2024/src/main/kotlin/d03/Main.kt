package d03

import java.io.File

fun main() {
    val code =
        File("src/main/kotlin/d03/input.txt")
            .readText()
            .replace("\n", "")

    val productSum = productSum(code)

    println("Part 01: $productSum") // 161 / 170068701

    val enabledProductSum =
        productSum(
            code
                // remove code between "don't()" and "do()"
                .replace(Regex("""don't\(\).*?do\(\)"""), "")
                // remove code after last "don't()"
                .replace(Regex("""don't\(\)(?!.*do\(\)).*${"$"}"""), ""),
        )

    println("Part 02: $enabledProductSum") // 48 / 78683433
}

fun productSum(code: String): Int {
    return Regex("""mul\((\d+),(\d+)\)""")
        .findAll(code).sumOf { match ->
            val (left, right) = match.destructured
            left.toInt() * right.toInt()
        }
}
