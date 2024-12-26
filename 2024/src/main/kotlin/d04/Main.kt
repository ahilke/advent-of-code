package d04

import java.io.File

fun main() {
    val rows = File("src/main/kotlin/d04/input.txt").readLines().map { it.toList() }

    val columns =
        rows.indices.map { row ->
            rows[row].indices.map { column ->
                rows[column][row]
            }
        }

    // 9,0
    // 8,0 9,1
    // 7,0 8,1 9,2
    // ...
    // 0,0 1,1 2,2 ...
    // 0,1 1,2 2,3 ...
    // 0,2 1,3 ...
    // ...
    // 0,9

    // e.g. for 10x10, we go: 9,0 - 8,0 9,1 - 7,0 8,1 9,2 ... 0,0 1,1
    // contains the middle diagonal
    val southEastDiagonalsLeft =
        rows.indices.reversed().map { row ->
            (0..<columns.size - row).map { step ->
                rows[row + step][step]
            }
        }

    // e.g. for 10x10, we go: 0,1 1,2 ... - 0,2 1,3 ... - 0,9
    val southEastDiagonalsRight =
        (1..<columns.size).map { column ->
            (0..<rows.size - column).map { step ->
                rows[step][column + step]
            }
        }

    val southEastDiagonals = southEastDiagonalsLeft + southEastDiagonalsRight
    val northWestDiagonals = southEastDiagonals.map { diagonal -> diagonal.reversed() }

    // 0.0
    // 0,1 1,0
    // 0,2 1,1 2,0
    // ...
    // 0,9 1,8 2,7 ...
    // 1,9 2,8 3,6 ...
    // 2,9 3,8 ...
    // ...
    // 9,9

    // e.g. for 10x10, we go: 0,0 - 0,1 1,0 - 0,9 1,8 2,7 ... 9,0
    // contains the middle diagonal
    val southWestDiagonalsLeft =
        columns.indices.map { column ->
            (0..column).map { step ->
                columns[column - step][step]
            }
        }

    // e.g. for 10x10, we go: 1,9 2,8 3,7 ... - 2,9 3,8 ... - 9,9
    val southWestDiagonalsRight =
        (1..rows.size).map { row ->
            (0..<rows.size - row).map { step ->
                rows[row + step][rows.size - step - 1]
            }
        }

    val southWestDiagonals = southWestDiagonalsLeft + southWestDiagonalsRight
    val northEastDiagonals = southWestDiagonals.map { diagonal -> diagonal.reversed() }

    printCharMatrix(rows)
    printCharMatrix(rows.map { line -> line.reversed() })
    printCharMatrix(southEastDiagonals)
    printCharMatrix(southWestDiagonals)
    printCharMatrix(northEastDiagonals)
    printCharMatrix(northWestDiagonals)

    val directions =
        listOf(
            rows,
            rows.map { line -> line.reversed() },
            columns,
            columns.map { line -> line.reversed() },
            southEastDiagonals,
            southWestDiagonals,
            northEastDiagonals,
            northWestDiagonals,
        )

    val count =
        directions.sumOf { direction ->
            direction.sumOf { line ->
                line.joinToString("")
                    .windowedSequence(size = 4, partialWindows = false)
                    .count { it == "XMAS" }
            }
        }

    println("Part 01: $count") // 18 / 2662
}

fun printCharMatrix(matrix: List<List<Char>>) {
    println(matrix.joinToString("\n") { it.joinToString("") })
    println("----")
}
