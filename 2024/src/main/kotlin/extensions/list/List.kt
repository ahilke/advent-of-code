package extensions.list

fun <T> List<T>.removeAt(index: Int): List<T> {
    return this.filterIndexed { j, _ -> j != index }
}
