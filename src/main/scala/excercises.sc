// 1
def last(ns: Seq[Any]): Option[Any] = {
  if (ns.isEmpty) None
  else Option(ns(ns.length - 1))
}
last(List(1, 1, 2, 3, 5, 8))
last(List('a', 'b'))

def last1(ns: Seq[Int]): Option[Int] = {
  if (ns.isEmpty) None
  else if (ns.size == 1) ns.headOption
  else last1(ns.tail)
}
last1(List(1, 2))

def last2(ns: Seq[Int]): Option[Int] = {
  ns match {
    case Nil => None
    case n :: Nil => Option(n)
    case _ => last2(ns.tail)
  }
}
// 2
def beforelast(ns: Seq[Int]): Option[Int] = {
  if (ns.isEmpty) None
  else if (ns.length == 1) None
  else Option(ns(ns.length - 2))
}
beforelast(List(1, 1, 2, 3, 5, 8))
beforelast(List())
beforelast(List(1, 2))

// 2*
def penultimate(ns: Seq[Int]): Option[Int] = {
  ns match {
    case Nil => None
    case _ :: Nil => None
    case m :: _ :: Nil => Option(m)
    case _ => penultimate(ns.tail)
  }
}
penultimate(List(1, 2, 3, 4, 5, 6))

// 3
def nth(ns: List[Int], n: Int): Option[Int] = {
  ns.drop(n).headOption
}
nth(List(1, 2, 3, 4, 5, 6), 3)
nth(List(1, 2, 3, 4, 5, 6), -70)

// 3*
def nthrec(ns: List[Int], n: Int): Option[Int] = {
  if (n > 0) {
    if (ns.isEmpty) {
      None
    } else nthrec(ns.tail, n - 1)
  } else ns.headOption
}
nthrec(List(1, 2, 3, 4, 5, 6), 3)

// 3**
def nthrecPM(ns: List[Int], n: Int): Option[Int] = {
  (ns, n) match {
    case (_, 0) => ns.headOption
    case (Nil, _) => None
    case _ => nthrecPM(ns.tail, n - 1)
  }
}
nthrecPM(List(1, 2, 3, 4, 5, 6), 4)

// 4
def len(ns: List[Int]): Int = {
  if (ns == Nil) 0
  else len(ns.tail) + 1
}
len(List(1, 2, 3))

// 4*
def len1(ns: List[Int]): Int = {
  ns match {
    case Nil => 0
    case _ => len1(ns.tail) + 1
  }
}
def lengthFunctional(ns: List[Int]): Int = ns.foldLeft(0) { (c, _) => c + 1 }

lengthFunctional(List(1, 2, 3))

// 5
def reverse(ns: List[Int]): List[Int] = {
  ns.foldLeft(List.empty[Int]) { (ns, n) => n :: ns }
}
reverse(List(1, 1, 2, 3, 5, 8))

// 5*
def reverseMatch(ns: List[Int]): List[Int] = {
  ns match {
    case Nil => ns
    case _ :: Nil => ns
    //    case m :: n :: Nil => n :: m :: Nil
    case _ => reverseMatch(ns.tail) :+ ns.head
  }
}
reverseMatch(List(1, 2, 3, 4, 5))

// 6
def palindrome(ns: List[Int]): Any = {
  //  ns == ns.foldLeft(List.empty[Int]) { (ns, n) => n :: ns }
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
palindrome(List(1, 1, 1, 1, 1))
palindrome(List(2, 1, 1, 1))
palindrome(List(2, 1, 1, 1, 2))
palindrome(List())
palindrome(List(5, 4, 3, 1, 3, 4, 5))

// 6*
def isPalindrome(ns: List[Int]): Boolean = {
  ns match {
    case Nil => false
    case _ :: Nil => true
    case m :: n :: Nil if m == n => true
    case _ if ns.head == ns.last => isPalindrome(ns.tail.init)
    case _ if ns.head != ns.last => false
  }
}
isPalindrome(List(2, 1, 1, 1))
isPalindrome(List())
isPalindrome(List(5, 4, 3, 1, 3, 4, 5))

// 7
def flat(as: List[Any]): List[Any] = {
  as.foldLeft(List.empty[Any]) { (result, lst) =>
    lst match {
      case n: Int => result :+ n
      case ns: List[_] => result ++ flat(ns)
      // type erasure ?
    }
  }
}
flat(List(List(1, 1), 2, List(3, List(5, 8))))

// 8
def compress(ns: List[Char]): List[Char] = {
  if (ns.length == 1) ns
  else ns.foldLeft(List.empty[Char]) { (result, n) =>
    n match {
      case n: Char if result.isEmpty => result :+ n
      case n: Char if n != result.last => result :+ n
      case n: Char if n == result.last => result
    }
  }
}

compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
compress(List('a', 'a'))

// 8*
def compress1(ns: List[Any]): List[Any] = {
  ns match {
    case Nil => Nil
    case h :: tail => h :: compress1(tail.dropWhile(_ == h))
  }
}
compress1(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))

// 9
def pack(ns: List[Char]): List[List[Char]] = {
  ns.foldLeft(List.empty[List[Char]]) { (css, c) =>
    if (css.isEmpty) List(List(c))
    else if (c == css.last.head) css.init ++ List(css.last :+ c)
    else css :+ List(c)
  }
}
pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))

// 10
def encode(ns: List[Char]): List[List[Any]] = {
  val pcns = pack(ns)
  pcns.foldLeft(List.empty[List[Any]]) { (result, ls) =>
    result :+ List((ls.length, ls.last))
  }
}

encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))

// 11
def encodeModified(ns: List[Char]): List[Any] = {
  val pcns = pack(ns)
  pcns.foldLeft(List.empty[Any]) { (result, ls) =>
    if (ls.length > 1) result :+ (ls.length, ls.last)
    else result :+ ls.last
  }
}

encodeModified(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))

// 12
//def decode(ns: List[List[Any]]): List[Char] = {
//
//}


// 14
def duplicate(ss: Seq[Char]): Seq[Char] = {
  ss.foldLeft(Seq.empty[Char]) { (result, s) =>
    result ++ Seq(s, s)
  }
}
duplicate(List('a', 'b', 'c', 'c', 'd'))

// 14*
def duplicateFM(ss: Seq[Char]): Seq[Char] = {
  ss.flatMap(c => Seq(c, c))
}

duplicateFM(List('a', 'b', 'c', 'c', 'd'))

// 15
def duplicateNtimes1(n: Int, ss: Seq[Char]): Seq[Char] = {
  ss.flatMap(c => (1 to n).map(num => c))

}

duplicateNtimes1(4, List('a', 'b', 'c', 'c', 'd'))

// 15*
def duplicateNtimes2(n: Int, ss: Seq[Char]): Seq[Char] = {
  ss.flatMap(c => Seq.fill(n)(c))
}

duplicateNtimes2(4, List('a', 'b', 'c', 'c', 'd'))

// 15**
def duplicateNtimes3(n: Int, ss: Seq[Char]): Seq[Char] = {
  for {
    c <- ss
    _ <- 0 until n
  } yield c
}
duplicateNtimes3(4, List('a', 'b', 'c', 'c', 'd'))

// Hello in a box
def prt(s: String): Unit = {
  println("+" + "-" * (s.length + 2) + "+")
  println(s"| $s |")
  println("+" + "-" * (s.length + 2) + "+")
}
prt("Hello")

def decipher(c: Char, n: Int): Char = {
  val charNum = c.toInt + n % 26
  if (charNum > 122) (charNum - 26).toChar
  else charNum.toChar
}

decipher('x', 22)

def caesar(s: String, n: Int): String = {
  s.toList.foldLeft(List.empty[Char]) { (result, c) =>
    result :+ decipher(c, n)
  }.mkString
}

caesar("jgorevxumxgsskx", 20)

def lastN(seq: Seq[Any], n: Int): Seq[Any] = {
  seq.foldLeft(Seq.empty[Any]) { (result, c) =>
    if (result.length < n) result :+ c
    else result.tail :+ c
  }
}

lastN(Seq(1, 2, 3, 4, 5), 3)

def hammingWeight(n: Int): Int = {
  val list = n.toString.map(_.asDigit).toList
  list.foldLeft(0) { (result, c) =>
    if (c == 0) result
    else result + 1

  }
}
hammingWeight(111111101)

