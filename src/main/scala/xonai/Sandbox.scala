package xonai

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Sandbox extends AnyFunSuite with Matchers {

  import Main._
  import Pretty.show

  def mkStringColumn(xs: String*) = {
    val sc = new StringColumn
    sc.length = xs.map(_.length).toArray
    sc.offset = Iterator.unfold(0 -> 0) {
      case (0, _)                               => Some(0 -> (1, 0))
      case (idx, off) if idx < sc.length.length =>
        val off2 = off + sc.length(idx - 1)
        Some(off2 -> (idx + 1, off2))
      case _                                    => None
    }.toArray
    sc.buffer = xs.flatMap(_.getBytes).toArray

    sc
  }

  test("mkStringColumn") {
    val sc = mkStringColumn("Hello", "HI", "A", "B", "hiThere")
    sc.length shouldBe Array(5, 2, 1, 1, 7)
    sc.offset shouldBe Array(0, 5, 7, 8, 9)
    sc.buffer shouldBe "HelloHIABhiThere".getBytes
    show(sc)
  }

  test("isEqualToA - something is null") {
    val sc = new StringColumn

    isEqualToA(1, sc) shouldBe false
  }

  test("isEqualToA - wrong length") {
    val sc = new StringColumn
    sc.offset = Array(0, 5, 7, 8, 9)
    sc.length = Array(5, 2, 1, 1)
    sc.buffer = Array("Hello", "HI", "A", "B", "hiThere").flatMap(_.getBytes)

    isEqualToA(1, sc) shouldBe false
  }

  test("isEqualToA - normal cases") {
    val sc = new StringColumn
    sc.offset = Array(0, 5, 7, 8, 9)
    sc.length = Array(5, 2, 1, 1, 3)
    sc.buffer = Array("Hello", "HI", "A", "B", "hiThere").flatMap(_.getBytes)

    isEqualToA(33, sc) shouldBe false // not exist in array
    isEqualToA(0, sc) shouldBe false  // longer
    isEqualToA(3, sc) shouldBe false  // not equal
    isEqualToA(2, sc) shouldBe true   // equal
  }

  test("isLikePromoSummer") {
    val testCases = Seq(
      ""               -> false,
      "a"              -> false,
      "whatever"       -> false,
      "PROM0SUMMER"    -> false,
      "1PROMOSUMMER"   -> false,
      "PROMOSUMMER2"   -> false,
      "PR_MOSUMMER"    -> false,
      "PROMOSUM_ER"    -> false,
      "PROMOSUMMER"    -> true,
      "PROMO SUMMER"   -> true,
      "PROMO123SUMMER" -> true
    )

    val sc = mkStringColumn(testCases.map(_._1): _*)
    show(sc)

    testCases.zipWithIndex.foreach { case ((_, expected), idx) =>
      isLikePromoSummer(idx, sc) shouldBe expected
    }
  }

  test("copyOfIndexes") {
    val sc = mkStringColumn("ab", "CDE", "f", "GHIJ", "q", "lkmk", "Z")
    val sc2 = copyAtIndexes(sc, Array(1, 3, 6), 3)
    show(sc)
    println()
    show(sc2)

  }

  test("whole composition (testQuery)") {
    testQuery()
  }

}
