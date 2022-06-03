// List(1, 1, 2, 3, 5, 8)
def reverse(ns: List[Int]): List[Int] = {
  ns.foldLeft(List.empty[Int]) { (ns, n) => n :: ns }
}
reverse(List(1, 1, 2, 3, 5, 8))