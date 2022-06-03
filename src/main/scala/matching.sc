def translate(n: Int): String = {
  if (n == 0) "zero"
  else if (n == 1) "jeden"
  else if (n == 2) "dwa"
  else "nieznane"
}

def translateMatch(n: Int): String = {
  n match {
    case 0 => "zero"
    case 1 => "jeden"
    case 2 => "dwa"
    // catch-all
    // default
    case _ => "nieznane"
  }
}

5 match {
  case 0 => false
  case _ => true
}

def ifMatch(n: Int): Boolean = {
  if (n == 0) false
  else true
}
ifMatch(5)

def ass(ns: Seq[Int]): Option[Int] = {
  ns.size match {
    case 0 => None
    case _ => Option(ns.last)
  }
}

ass(List(1, 2, 3, 4, 5))


def reverseMatch(ns: List[Int]): List[Int] = {
  ns match {
    case Nil => ns
    case _ :: Nil => ns
    case m :: n :: Nil => n :: m :: Nil
    case _ => reverseMatch(ns.tail) :+ ns.head
  }
}
reverseMatch(List(1, 2, 3, 4, 5))


List(1, 2, 3, 4, 5).drop(3)