//import scala.collection.mutable.ArrayBuffer
//case class MyList(elem: Int) {
//  val list: ArrayBuffer[Int] = new ArrayBuffer[Int]()
//  list.addOne(elem)
//
//  def myadd(n: Int*): MyList = {
//    n.foreach(list.addOne)
//    this
//  }
//
//  def mytail: MyList = {
//    val x = list.tail
//    val pl = MyList(x.head)
//    x.tail.foldLeft((pl)) { (result, e) =>
//      result.myadd(e)
//    }
//  }
//
//  def myhead: Int = {
//    list.head
//  }
//
//  def myforeach(f: Int => Unit): Unit = {
//    //    list.foreach()
//    //    val f = list.foreach()
//    ???
//  }
//
//  override def toString = {
//    s"MyList(${list.sorted.mkString(",")})"
//  }
//}
//
//
//MyList(1).myadd(1,2).mytail
//MyList(3).myadd(1,2, 4, 6).mytail
//MyList(1).myadd(1).mytail.myhead
//
