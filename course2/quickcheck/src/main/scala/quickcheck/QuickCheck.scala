package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, empty)
    val h2 = insert(a, h)
    findMin(h2) == Math.min(a, b)
  }

  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("del2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(deleteMin(h)) == Math.max(a, b)
  }

  property("del3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(deleteMin(h))) == Math.max(c, Math.max(a, b))
  }

  property("meld2") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("meld1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(meld(h, empty)))
  }

  property("asending") = forAll { h: H =>
    def checkAsend(h: H, prev: Int): Boolean = {
      if (isEmpty(h)) true
      else prev <= findMin(h) && checkAsend(deleteMin(h), findMin(h))
    }
    checkAsend(deleteMin(h), findMin(h))
  }
}
