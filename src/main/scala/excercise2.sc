// 1
def last(ns: List[Int]): Option[Int] = {
  if (ns == Nil) None
  else Option(ns(ns.length - 1))
}

last(List(1, 2, 3, 4))

// 2
def penultimate(ns: List[Int]): Option[Int] = {
  ns match {
    case Nil => None
    case _ :: Nil => None
    case n :: _ :: Nil => Option(n)
    case _ => penultimate(ns.tail)

  }
}

penultimate(List(1, 2, 3, 4))

// 3
def nth(ns: List[Int], n: Int): Option[Int] = {
  ns.drop(n).headOption
}

nth(List(1, 2, 3, 4), 2)

// 4
def length(ns: List[Int]): Int = {
  if (ns == Nil) 0
  else length(ns.tail) + 1
}
length((List(1, 2, 3, 4)))

// 5
def reverse(ns: List[Int]): List[Int] = {
  ns.foldLeft(List.empty[Int]) { (ns, n) => n :: ns }
}
reverse((List(1, 2, 3, 4)))

// 6
def isPalindrome(ns: List[Int]): Boolean = {
  if (ns.length % 2 == 0) {
    val ns2 = ns.drop(ns.length / 2)
    val ns1 = ns diff ns2
    ns1 == ns2
  } else {
    val nsod2 = ns.drop(ns.length / 2)
    val nsod1 = ns diff nsod2.tail
    nsod1 == nsod2
  }
}
isPalindrome(List())
isPalindrome(List(1, 2, 1))
isPalindrome(List(1, 1, 1))
isPalindrome(List(5, 4, 3, 2, 1, 2, 3, 4, 5))
isPalindrome(List(2, 1, 1, 2))
isPalindrome(List(2, 1, 1))
isPalindrome(List(1, 1, 1, 3))

// 7
def flatten(ns: List[Any]): List[Any] = {
  ns.foldLeft(List.empty[Any]) { (result, lst) =>
    lst match {
      case n: Int => result :+ n
      case ns: List[_] => result ++ flatten(ns)
    }
  }
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

