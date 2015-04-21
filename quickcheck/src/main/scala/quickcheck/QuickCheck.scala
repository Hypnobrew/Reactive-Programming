package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert_empty") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("insert") = forAll { (a: Int, b: Int) =>
    val first = insert(b, insert(a, empty))
    val second = insert(a, insert(b, empty))
    if (a < b) {
      findMin(first) == a && findMin(second) == a
    } else {
      findMin(first) == b && findMin(second) == b
    }
  }

  property("sorted_seq") = forAll { (h: H) =>
    def pop(h: H, last: Int): Boolean = {
      val min = findMin(h)
      val h2 = deleteMin(h)
      last <= min && (isEmpty(h2) || pop(h2, min))
    }
    isEmpty(h) || pop(h, Int.MinValue)
  }

  property("meld_min") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val min = findMin(h)
    min == findMin(h1) || min == findMin(h2)
  }

  property("insert_list") = forAll { l: List[Int] =>
    def pop(h: H, l: List[Int]): Boolean = {
      if (isEmpty(h)) {
        l.isEmpty
      } else {
        !l.isEmpty && findMin(h) == l.head && pop(deleteMin(h), l.tail)
      }
    }
    val sl = l.sorted
    val h = l.foldLeft(empty)((he, a) => insert(a, he))
    pop(h, sl)
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k,m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
