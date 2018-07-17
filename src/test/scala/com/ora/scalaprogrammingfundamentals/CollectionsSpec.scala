package com.ora.scalaprogrammingfundamentals

import java.time.LocalDate

import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.{HashSet, LinearSeq, Queue, TreeSet}
import scala.collection.mutable.ArrayBuffer

class CollectionsSpec extends FunSuite with Matchers {

  /*
   * For performance characteristics of each of the
   * following collections, see:
   * https://bit.ly/2pQPXRu
   */

  test(
    """We start our journey into collections with a List.
      |  An ordered collection. Per documentation this is
      |  suited for lifo stack like patterns""".stripMargin) {
    val list = List.apply(40, 50, 60)
    val list2 = list :+ 70 //immutable
    val list3 = 1 +: list2 :+ 100

    list should contain inOrder(40, 50, 60)
    list2 should contain inOrder(40, 50, 60, 70)
    list3 should contain inOrder(1, 40, 50, 60, 70, 100)
  }

  test("""To get the the 2nd element of the list we use apply.""") {
    val list = List.apply(40, 50, 60)
    list.apply(2) should be(60)
    list should have size 3
  }

  test(
    """To create a collection, there are varying signatures in
      |  the companion object of many of the collections that
      |  we will be talking about including a list.""".stripMargin) {
    val list = List.fill(3)(() => LocalDate.now)
    list.size should be(3)
  }

  test(
    """For most purposes a Vector is preferable, fast random access,
      |  append, prepend, and updates. Lucky for us the methods
      |  are very much the same""".stripMargin) {
    val vector = Vector(10, 11, 19)
    val vector2 = vector :+ 40
    val vector3 = 9 +: vector2
    vector3 should contain inOrder(9, 10, 11, 19, 40)
  }

  test(
    """A range can be created with either to, until,
      |  or explicitly with a range""".stripMargin) {
    val range1 = 1 to 5
    range1 should contain inOrder(1, 2, 3, 4, 5)
    val range2 = 1 until 5
    range1 should contain inOrder(1, 2, 3, 4)
    val range3 = Range.apply(1, 5)
    range3 should contain inOrder(1, 2, 3, 4)
    val range4 = Range.inclusive(1, 5)
    range4 should contain inOrder(1, 2, 3, 4, 5)
    val range5 = Range.apply(1, 10, 2)
    range5 should contain inOrder(1, 3, 5, 7, 9)
  }

  test(
    """If you have tried Scala, you have definitely encountered a Seq.
      |  A Seq is a base trait for sequences which are iterables with order.
      |  There are two subtraits of a Seq:
      |     - IndexedSeq : fast random access, fast length
      |     - LinearSeq : fast head, fast tail, fast empty operations
      |  The current default Seq when instantiated is a List
      |  The current default IndexedSeq is a Vector
      |  The current default LinearSeq is a List""".stripMargin) {

    Seq(30, 50, 100) shouldBe a[List[_]]
    IndexedSeq(30, 50, 100) shouldBe a[Vector[_]]
    LinearSeq(30, 50, 100) shouldBe a[List[_]]
  }

  test(
    """A stream can be a finite or an infinite collection.
      |  The construction can
      |  be done using recursion""".stripMargin) {
    def continuousEvens(): Stream[BigInt] = {
      def ce(n:BigInt):Stream[BigInt] = Stream.cons(n, ce(n + 2))
      ce(2)
    }
    continuousEvens().take(5) should contain inOrder(2, 4, 6, 8, 10)
  }

  test("""Another way we can write the above Stream is using is using the
      |  #:: operator""".stripMargin) {
    def continuousEvens(): Stream[BigInt] = {
      def ce(n:BigInt):Stream[BigInt] = n #:: ce(n + 2)
      ce(2)
    }
    continuousEvens().take(5) should contain inOrder(2, 4, 6, 8, 10)
  }

  test(
    """An array is a java based array with a Scala wrapper
      |  around it to perform the functionality""".stripMargin) {
    val arr = Array(1, 2, 3, 4, 5)
    arr.reverse should be(Array(5, 4, 3, 2, 1))
    arr.getClass.getSimpleName should be("int[]")
  }

  test(
    """Whereas a List is a LIFO, a Queue is a FIFO just like a
      |  line at a grocery store. The queue internally has two sections,
      |  an in collection and an out collection. When out runs out, out
      |  becomes an in.reverse and out replaced with Nil. This is called a
      |  pivot""".stripMargin) {

    val queue = Queue.apply(1, 2, 3, 4)
    val result = queue.enqueue(5)
    result should be(Queue.apply(1, 2, 3, 4, 5))
    result.dequeue should be(1, Queue.apply(2, 3, 4, 5))
  }

  test(
    """A Set is a collection with unique items with no order. They can
      |  either be as HashSet which is stored is special type of collection
      |  called trie. This will have fast lookup, add, and remove""".stripMargin)
  {
    val set = HashSet(1, 2, 3, 4, 5, 6, 8)
    set.contains(4) should be(true)
    set.apply(8) should be(true) //huh?
    set.apply(10) should be(false) //huh?
  }

  test(
    """A Set is can also be a TreeSet.
      |  This will have logarithmic performance. It will win
      |  generally in performance when you need to
      |  find the smallest element
      |  of the set. As far as API is concerned
      |  you should expect no difference.""".stripMargin) {
    val set = TreeSet(1, 2, 3, 4, 5, 6, 8)
    set.contains(4) should be(true)
    set.apply(8) should be(true) //huh?
    set.apply(10) should be(false) //huh?
  }

  test("""A Map is a collection of pairs, also known as Tuple2""".stripMargin) {
    val map = Map(1 -> "One", 2 -> "Two", 3 -> "Three")
    map.get(2) should be(Some("Two"))
    val result = map + (4 -> "Four")
    result.get(4) should be(Some("Four"))

    a[NoSuchElementException] should be thrownBy { //ScalaTest
      result.apply(10)
    }
  }

  test(
    """There are also mutable collections. Though much of the Scala
      |  programmers like to generally use immutable collections at times
      |  you may want to use mutable collections for efficiency though it
      |  is generally good form to lock collections immutably when returning
      |  from a method. Here is a ListBuffer which allows you to create a
      |  mutable List changing what you need. Note the API differences
      |  with immutable.""".stripMargin) {

    val ab = ArrayBuffer[Int](10, 20)
    ab += 30
    ab += 40
    ab.prepend(5)

    ab should be(ArrayBuffer(5, 10, 20, 30, 40))
  }
}
