import scala.io.Source

object source_excercises extends App {
  val filename = args(0)

  def nth(ns: Iterator[String], n: Int): Iterator[String] = {
    if (n == 1) ns.take(1)
    else ns.drop(n - 1).take(1)
  }

//  def reverseMap(m: Map[String, Int]) = for ((k, v) <- m) yield (v, k)

  val num_lines = Source.fromFile(filename).getLines.size

  def line_to_list(s: String, n: Int): Seq[String] = {
    nth(Source.fromFile(s).getLines, n).flatMap(line => line.split("\\s+")).toSeq
  }

//    val chain_of_commands = for {
//      i <- 1 to num_lines
//    } println(line_to_list(filename, i))

  line_to_list(filename,1)

}

//  val firstline = Source.fromFile(filename).getLines.take(1).flatMap(line => line.split("\\s+")).toSeq
//Source.fromFile(s).getLines.take(n).flatMap(line => line.split("\\s+")).toSeq
//reverseMap(Source.fromFile(s).getLines.zipWithIndex.toMap)(n).flatMap(line => line.split("\\s+")).toSeq
//val x = Source.fromFile(s).getLines.flatMap(line => line.split("\\s+")).toSeq
//reverseMap(x.zipWithIndex.toMap)(n).toList

//nth(Source.fromFile("C:\\Users\\m.sabatowski\\IdeaProjects\\99_problems\\text.txt").getLines, n).flatMap(line => line.split("\\s+")).toSeq