import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PathUtilsSpec extends AnyFlatSpec with should.Matchers {
  import PathUtils._
  ignore should " " in {


    findFile("kubectl") shouldBe Seq("/usr/local/bin")

  }
  "myzip" should "intertwine two sequences" in {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq('a', 'a', 'a')
    val expected = Seq(1, 'a', 2, 'a', 3, 'a')

    myzip(s1, s2) shouldBe expected
  }

  "myzip" should "intertwine two sequences of different size, first is shorter" in {
    val s1 = Seq(1, 2)
    val s2 = Seq('a', 'a', 'a')
    val expected = Seq(1, 'a', 2, 'a', 'a')

    myzip(s1, s2) shouldBe expected
  }
  "myzip" should "intertwine two sequences of different size" in {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq('a', 'a')
    val expected = Seq(1, 'a', 2,'a', 3)

    myzip(s1, s2) shouldBe expected
  }

}
