package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = Gen.oneOf(
    const(empty),
    for {
      e <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(e, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("two int into an empty heap") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val minimum = a min b
    findMin(h) == minimum
  }

  property("insert into an empty heap then delete") = forAll { e: Int =>
    val h = insert(e, empty)
    deleteMin(h) == empty
  }

  property("keep deleting and get a sorted sequence") = forAll { h: H =>
    def isSorted(heap: H): Boolean = heap match {
      case e if isEmpty(e) => true
      case _ =>
        val m = findMin(heap)
        val res = deleteMin(heap)
        if (isEmpty(res)) true
        else if (m > findMin(res)) false
        else isSorted(res)
    }
    isSorted(h)
  }

  property("merge 2 heap") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> (findMin(meld(h1, h2)) == (findMin(h1) min findMin(h2)))
  }

  def getSorted(heap: H): List[Int] = heap match {
    case e if isEmpty(e) => List[Int]()
    case _ =>
      val m = findMin(heap)
      val res = deleteMin(heap)
      m :: getSorted(res)
  }
  def insertSort(hh1: H, hh2: H): List[Int] = (hh1, hh2) match {
    case (e1, e2) if (isEmpty(e1) && isEmpty(e2)) => List[Int]()
    case (e1, _) if isEmpty(e1) => getSorted(hh2)
    case (_, e2) if isEmpty(e2) => getSorted(hh1)
    case _ =>
      val m1 = findMin(hh1)
      val m2 = findMin(hh2)
      if (m1 < m2) m1 :: insertSort(deleteMin(hh1), hh2)
      else m2 :: insertSort(hh1, deleteMin(hh2))
  }


  property("merge action integrity") = forAll { (h1: H, h2: H) => (h1, h2) match {
    case (e1, e2) if (isEmpty(e1) || isEmpty(e2)) => true
    case _ => getSorted(meld(h1, h2)) == insertSort(h1, h2)
  }
  }
}
