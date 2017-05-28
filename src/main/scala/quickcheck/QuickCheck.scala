package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v,h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") =
    forAll { (i: Int, j: Int) =>
      val min = if (i > j) j else i
      findMin(insert(i,insert(j,empty))) == min && findMin(insert(j,insert(i,empty))) == min
  }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") =
    forAll { (e: Int) =>
      deleteMin(insert(e,empty)) == empty
  }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)") =
    forAll { (h: H) =>
      def helper(acc: List[Int], h: H): List[Int] =
        if (isEmpty(h)) acc else helper(findMin(h)::acc,deleteMin(h))
      val res = helper(List(),h)
      res.reverse == res.sorted
  }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") =
    forAll { (h1: H, h2: H) =>
      val hT = meld(h1,h2)
      if (isEmpty(h1)) {
        if (isEmpty(h2)) true else findMin(hT) == findMin(h2)
      } else {
        if (isEmpty(h2)) findMin(hT) == findMin(h1) else findMin(hT) == findMin(h1) || findMin(hT) == findMin(h2)
      }
  }

  property("The greater of the minimum of each heap must be contained in the melded list") = {
    forAll { (h1: H, h2: H) =>
      def helper(m2: Int, h: H): Boolean =
        if (isEmpty(h)) false else { if (findMin(h) == m2) true else  helper(m2, deleteMin(h))}
      if (!isEmpty(h1) && !isEmpty(h2)) {
        val m2 = if (findMin(h1) < findMin(h2)) findMin(h2) else findMin(h1)
        helper(m2, deleteMin(meld(h1,h2)))
      } else true
    }
  }

}

object main extends App {
  val bh =  new QuickCheckHeap with BinomialHeap

  for (_ <- (1 to 10)) yield {println(bh.genHeap.sample)}
}
