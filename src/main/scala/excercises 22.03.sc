import scala.io.Source

val letters = List('A', 'B', 'C', 'D', 'F', 'G', 'H')
val index = 1 to 8

def checkerboard(l: List[Char], i: Range): List[Any] = {
  for (x <- l;
       y <- i)
  yield (x, y)
}
checkerboard(letters, index)

def checkerdiag(cs: List[Char], ind: Seq[Int]): List[Any] = {
  for {
    x <- cs
    y <- ind
    if x - y >= 64
  } yield (x, y)
}
checkerdiag(letters, index)


def textcounter(filename: String): Map[String, Int] = {
  val words = Source.fromFile(filename).getLines().flatMap(line => line.split(" ")).toSeq
  words.groupBy(identity).map(t => (t._1, t._2.length))

  //  words.foldLeft(Map.empty[String, Int]) { (map, word) =>
  //    map + (word -> (map.getOrElse(word, 0) + 1))

}

}
textcounter("C:\\Users\\m.sabatowski\\IdeaProjects\\99_problems\\text.txt")


