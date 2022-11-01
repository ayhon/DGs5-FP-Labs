package quickcheck

import org.scalacheck.*
import scala.annotation.tailrec
import Arbitrary.*
import Gen.*
import Prop.forAll
import scala.annotation.constructorOnly

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] =
    for
      k <- arbitrary[Int]
      m <- oneOf(const(empty),genHeap)
    yield insert(k,m)
  given Arbitrary[H] = Arbitrary(genHeap)
  
  // The only element of a heap is its minimum value
  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // Adding and finding the minimal element shouldn't change minimal element
  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  // If you insert any two elements into an empty heap, finding the minimum 
  // of the resulting heap should get the smallest of the two elements back.
  property("min-of-2") = forAll{ (a: Int, b: Int) =>
    val min = if a < b then a else b
    findMin(insert(a,insert(b,empty))) == min
  }
   
  // If you insert an element into an empty heap, then delete the minimum,
  //  the resulting heap should be empty.
  property("del-one-elem") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a,empty)))
  }

  // Given any heap, you should get a sorted sequence of elements when 
  // continually finding and deleting minima. 
  property("elements-come-out-sorted") = forAll{ (h: H) =>
    def list_elements(h: H): List[Int] = 
      @tailrec
      def list_elements_tr(h: H, acc: List[Int]): List[Int] = 
        if isEmpty(h) then acc
        else list_elements_tr(deleteMin(h),findMin(h) :: acc)
      list_elements_tr(h,Nil).reverse

    list_elements(h) == list_elements(h).sorted
  }
  
  // Given any list of elements, putting them in a heap and getting
  // them out should sort them
  property("heap-sort") = forAll { (ls: List[Int]) =>
    def construct_heap(ls: List[Int]): H = ls match
      case Nil => empty
      case x::xs => insert(x,construct_heap(xs))

    def list_elements(h: H): List[Int] = 
      @tailrec
      def list_elements_tr(h: H, acc: List[Int]): List[Int] = 
        if isEmpty(h) then acc
        else list_elements_tr(deleteMin(h),findMin(h) :: acc)
      list_elements_tr(h,Nil).reverse
    
    val h = construct_heap(ls)
    ls.sorted == list_elements(h)
  }

  // Finding a minimum of the melding of any two heaps should return a minimum
  // of one or the other.
  property("min-when-melded") = forAll{ (h1: H, h2: H) =>
    val m1: Int = findMin(h1)
    val m2: Int = findMin(h2)
    val h: H = meld(h1,h2)
    findMin(h) == m1 || findMin(h) == m2
  }
  
  ///////////////////////////// MY PROPERTIES ////////////////////////////////

  // When melding two heaps with only one element, the new heap should contain
  // both elements
  property("meld-preserves-elements") = forAll{ (a: Int, b: Int) =>
    // (3 should fail because it is getting rid of the elements of one array)
    val h1 = insert(a,empty)
    val h2 = insert(b,empty)
    val h = meld(h1,h2)
    
    def list_elements(h: H): List[Int] = 
      @tailrec
      def list_elements_tr(h: H, acc: List[Int]): List[Int] = 
        if isEmpty(h) then
          acc
        else
          list_elements_tr(deleteMin(h),findMin(h) :: acc)
      list_elements_tr(h,Nil).reverse
    
    (list_elements(h) contains a) && (list_elements(h) contains b)
  }
  
  // When we delete the minimum from a heap, it should have one less element
  property("deleteMin-one-less") = forAll { (h: H) =>
    def num_elements(h: H) =
      @tailrec
      def num_elements_tr(h: H, acc: Int): Int =
        if isEmpty(h) then acc else num_elements_tr(deleteMin(h), acc + 1)
      num_elements_tr(h,0)
    
    num_elements(deleteMin(h)) == num_elements(h) - 1
  }
  
  // In an array of 2 elements, deleteMin deletes the minimal element
  property("deleteMin") = forAll { (a: Int, b: Int) =>
    val max = if a > b then a else b
    val h = insert(a,insert(b,empty))
    findMin(deleteMin(h)) == max
  }