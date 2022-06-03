import java.io.File

object PathUtils {
  def getPathEnv: String = sys.env("PATH")

  def findFile(file: String): Seq[String] = {
    def isExists(dir: String) = {
      new File(dir + File.separatorChar + file).exists()
    }

    getPathEnv.split(File.pathSeparatorChar).filter(isExists)
  }

  def myzip(s1: Seq[Any], s2: Seq[Any]): Seq[Any] = {
    def x(first: Seq[Any], second: Seq[Any]): (Seq[Any], Seq[Any], Seq[Any]) = {
      ???
    }
    if (s1.length > s2.length) {
      val eq1 = s1.dropRight(s1.length - s2.length)
      eq1.zip(s2).flatMap(_.productIterator) :+ s1.tail(s1.length - s2.length)
    }
    else if (s2.length > s1.length) {
      val eq2 = s2.dropRight(s2.length - s1.length)
      //      val x1 = eq2.zip(s1).flatMap{ case (a, b) => Seq(a,b) }
      val x1 = s1.zip(eq2).flatMap(_.productIterator)
      val x2 = s2.tail(s2.length - s1.length)
      val x3 = x1 :+ x2
      x3
    }
    else s1.zip(s2).flatMap(_.productIterator)
    s1.zip(s2).flatMap(_.productIterator)
  }
}


