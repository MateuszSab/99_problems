class Logger(prefix: String) {
  def print(message: String) = prefix + message
}

object Main extends App {
  val pre = new Logger("pre-")
  println(pre.print("sokratyk"))

  val jacek = new Logger("[Jacek] ")
  println(jacek.print("witaj świecie"))

  val cos = new Logger(">>> ")
  println(cos.print("coś nowego"))

  (1 to 3).map(_.toString).foreach(jacek.print)

}
