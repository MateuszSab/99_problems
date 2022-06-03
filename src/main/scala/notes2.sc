def listOfOdds(n: Int): List[Int] = List.range(0, n * 2).filter(_ % 2 != 0)

listOfOdds(20)

def listOfOdds2(n: Int): List[Int] = ((0 until n).map(_ * 2 + 1)).toList

listOfOdds2(20)

def fac(n: Int): List[Int] = List.range(2, n).filter(n % _ == 0)

fac(35)

def facList(ns: List[Int]): List[Int] = ns.flatMap(fac)

facList(List(9, 11, 13, 15))

def myTake(ns: List[Char], n: Int): List[Char] = {
  ns.foldLeft(List.empty[Char]) { (result, x) =>
    if (n == 0) Nil
    else if (result.isEmpty) List(x)
    else if (result.length < n) result :+ x
    else result
  }
}

myTake(List('a', 'b', 'c', 'd', 'f', 'g', 'h'), 0)
myTake(List(), 5)

def myTake1(ns: List[Char], n: Int): List[Char] = {
  val count = ns.length - n
  if (count > 0) {
    if (ns.isEmpty) Nil
    else myTake1(ns.init, count - 1)
  } else ns
}

myTake1(List('a', 'b', 'c', 'd', 'f', 'g', 'h'), 4)

def myTake2(ns: List[Char], n: Int): List[Char] = {
  val count = ns.length - n
  if (ns.isEmpty) Nil
  else if (count > 0) myTake2(ns.init, count - 1)
  else ns
//  if (count > 0) {
//    if (ns.isEmpty) Nil
//    else myTake1(ns.init, count - 1)
//  } else ns
}

myTake2(List('a', 'b', 'c', 'd', 'f', 'g', 'h'), 4)

import scala.util.Random

val seq1 = Seq(1, 2, 3, 4, 5)
val seq2 = Seq(6, 7, 8, 9, 10)


def randpairs(ns1: Seq[Int], ns2: Seq[Int]): Seq[(Int, Int)] = {
  Random.shuffle(ns1).zip(Random.shuffle(ns2))
}
randpairs(seq1, seq2)

//def randpairs1(ns1: Seq[Int], ns2: Seq[Int]): Seq[(Int, Int)] = {
//  for (i <- Random.shuffle(ns1).indices)
//    yield (i, ns2.indices)
//
//
//}
//randpairs1(seq1, seq2)

def randpairs[T](ns1: Seq[T], ns2: Seq[T]): Seq[(T, T)] = {
  Random.shuffle(ns1).zip(Random.shuffle(ns2))
}
randpairs(seq1, seq2)


//def lsort(llc: List[List[Char]]): List[List[Char]] = {
//
//}

// 28
val x = List(List('a', 'b', 'c'), List('d', 'e'), List('f', 'g', 'h'), List('d', 'e'), List('i', 'j', 'k', 'l'), List('m', 'n'), List('o'))

val y = x.map(l => l :+ l.length)

x.sortBy((x => (x.length, x.head)))


// 23.03
def consecutiveMultiplicates[T](ts: Seq[T], n: Int): Seq[T] = {

  ts.foldLeft((Seq.empty[T], 1)) { case ((result, counter), t) =>
    val cnt = if (counter % n == 0) 1 else counter + 1
    (result ++ Seq.fill(counter)(t), cnt)
  }._1
}

consecutiveMultiplicates(Seq(1, 2, 3, 4, 5), 3)


