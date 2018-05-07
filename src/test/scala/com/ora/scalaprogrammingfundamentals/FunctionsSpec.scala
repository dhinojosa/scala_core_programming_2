package com.ora.scalaprogrammingfundamentals

import org.scalatest.{FunSuite, Matchers}

class FunctionsSpec extends FunSuite with Matchers {
  test(
    """As a reminder from all those that took the beginner's course,
      |  a function really is an anonymous
      |  instantiation of a trait.""".stripMargin) {

    val f = new Function[CharSequence, AnyVal] {
      override def apply(v1: CharSequence): AnyVal = v1.length
    }

    f("Hello") should be(5)
  }

  test("The above can be whittled down to the following:") {
    val f = (s: String) => s.length
    f.apply("Zanzibar") should be(8)
  }

  test(
    """If you declare the left hand side you can
      |  do some nifty tricks on the right hand side:""".stripMargin) {
    val f: String => Int = s => s.length
    f.apply("Andorra") should be(7)
  }

  test(
    """Also, since the left hand side, has all the type information,
      |  on the right hand side you can trim the left hand side
      |  with syntactical tricks like use the placeholder""".stripMargin) {
    val f: String => Int = _.length
    f.apply("Andorra") should be(7)
  }

  test(
    """If the type system has enough information either because
      |  of the left hand side of an assignment, or a parameter in method, or
      |  the way generic types are situated within a class,
      |  you can get rid of some additional code. In the following example, you
      |  can drop the underline and leave 5+, but will come with some warnings
      |  that you can turn off with """.stripMargin) {

    val f: Int => Int = 5 + _
    f.apply(3) should be(8)
  }

  test(
    """A closure is a function that will "wrap" or "close"
      |  around an outside value.""".stripMargin) {
    def createFunction(i: Int): Int => Int = {
      (x: Int) => x + i
    }

    Vector(1, 2, 3).map(createFunction(5)) should contain inOrder(6, 7, 8)
  }

  test(
    """Closure have some particular implications. One such implication
      |  is called currying. Currying will break a function of one or
      |  more arguments into parts so that they can be applied
      |  partially""".stripMargin) {

    def foo(f: Int => Int) = f(40)

    val f: (Int, Int, Int) => Int = (x: Int, y: Int, z: Int) => x + y + z
    val fc: Int => Int => Int => Int = f.curried
    val f1: Int => Int => Int = fc(3)
    val f2: Int => Int = f1(4)
    val f3: Int = f2(10)
    f3 should be(17)

    val manuallyCurried = (x: Int) => (y: Int) => (z: Int) => x + y + z
    manuallyCurried(3)(4)(10) should be(17)
  }

  test(
    """Compose is the equivalent of f(g(x)).  But what this is takes
      |  some perspective.  g(x) should be evaluated first and then the
      |  result of that (let's call it a) should be applied to f,
      |  therefore f(a) to get result b.  But these functions can be
      |  applied together to form one cohesive function""".stripMargin) {

    pending
    val tupleFirst = (t: (String, Int)) => t._1
    val getFirstThreeLetters = (s: String) => s.substring(0, 3)

  }

  test(
    """andThen is g(f(x)).  f(x) is applied first and
      |  then g is then applied. In the following example we
      |  recreate the compose but using andThen""".stripMargin) {

    pending
    val tupleFirst = (t: (String, Int)) => t._1
    val getFirstThreeLetters = (s: String) => s.substring(0, 3)
  }

  test("""Map will apply the given function on all elements of a
      |  Traversable and return a new collection
      |  of the result.""".stripMargin) {
    val vector = Vector(1, 3, 4, 6)
    val result = vector.map(x => x * 4)
    result should be(List(4, 12, 16, 24)) //4
  }

  test("""Map can be applied to a Stream, it is just another collection""") {
    Stream
      .from(1, 2)
      .map(x => x * 5)
      .take(4)
      .toVector should contain inOrder(5, 15, 25, 35)
  }

  test("""Map in an Option, although an Option is not a collection,
      |  it is has some of the same attributes like map that will operate
      |  with its internals. To apply a map to a None will just render
      |  a None""".stripMargin) {
    Some(10).map(x => x + 40) should be(Some(50))
    None.asInstanceOf[Option[Int]].map(x => x + 40) should be(None)
  }

  test(
    """We can also use a map on a scala Map, you have two choices,
      |  either map which takes the Tuples, or mapValues which just
      |  maps over the values.""".stripMargin) {
    pending
    val mapStructure = Map(1 -> "One", 2 -> "Two", 3 -> "Three")
  }

  test("""foldLeft will take two parameters group, the first
      |  will contain a seed and then a function that will
      |  aggregate the collection into one.""".stripMargin) {
      pending
  }

  test("""reduce will collapse all elements of a collection using a function.
      |  It will start the first element as the 'seed' or 'accumulation"""
      .stripMargin) {
      pending
  }

  test(
    """flatMap will not only apply the given function on all
      |  elements of a Traversable,
      |  but all elements within the elements
      |  and flatten the results""".stripMargin) {
      pending
  }

  test("""flatMap of Options will filter out all Nones and Keep the Somes""") {
    val list = List(1, 2, 3, 4, 5)
    val result = list.flatMap(it => if (it % 2 == 0) Some(it) else None)
    result should be(List(2, 4))
  }

  test("""foreach will apply a function to all elements of a Traversable,
      |  but unlike the map function, it will not return anything
      |  since the return type is Unit, which
      |  is like a void return type in Java, C++""".stripMargin) {
    pending
  }

  test("""groupBy will categorize a collection by a function, and return a
      |  map where the keys were derived by that function""".stripMargin) {
    pending
  }

  test("""mkString will create a string from a
      |  collections elements, and offers
      |  multiple ways to do so""".stripMargin) {
    pending
  }

  test("""collect will apply a partial function to all elements
          |  and will return a different collection.""".stripMargin) {
    pending
  }

  test("""scan is like a reduce but maintains a running total
      |  with each iteration""".stripMargin) {
    pending
  }

  test("""zip will interweave two collections together leaving a tuple""") {
    pending
  }

  test("""view will not immediately evaluate a chain until a terminal
      |  operation is called, like reduce, count, or force""".stripMargin) {
    pending
  }

  test("""sorted will sort the collection based on an implicit ordering
      |  and return that ordered collection""".stripMargin) {
    pending
  }

  test("""sortBy will also sort the collection based on an
      |  implicit rule, but will apply a function first""".stripMargin) {
    pending
  }
}
