def hammingWeight(n: Int): Int = {
  val list = n.toString.map(_.asDigit).toList
  list.foldLeft(0) { (result, c) =>
    if (c == 0) result
    else result + 1

  }
}
hammingWeight(111111101)