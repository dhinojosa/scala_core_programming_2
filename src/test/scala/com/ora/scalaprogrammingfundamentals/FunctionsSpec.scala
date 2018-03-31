package com.ora.scalaprogrammingfundamentals

import org.scalatest.{FunSuite, Matchers}

//dhinojosa@evolutionnext.com

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

    val tupleFirst: ((String, Int)) => String = (t: (String, Int)) => t._1
    val getFirstThreeLetters = (s: String) => s.substring(0, 3)

    val newFunction: ((String, Int)) => String =
      getFirstThreeLetters.compose(tupleFirst)
    newFunction("Fellow" -> 100) should be("Fel")
  }

  test(
    """andThen is g(f(x)).  f(x) is applied first and
      |  then g is then applied. In the following example we
      |  recreate the compose but using andThen""".stripMargin) {

    val tupleFirst = (t: (String, Int)) => t._1
    val getFirstThreeLetters = (s: String) => s.substring(0, 3)

    val newFunction: ((String, Int)) => String = tupleFirst
      .andThen(getFirstThreeLetters)
    newFunction.apply("Fellow", 100) should be("Fel")
  }

  test(
    """map will apply the given function on all elements of a
      |  Traversable and return a new collection of the result.""") {
    val vector = Vector(1, 3, 4, 6)
    val result = vector.map(x => x * 4)
    result should be(List(4, 12, 16, 24)) //4
  }

  test(
    """map can be applied to a Stream""") {
    Stream
      .from(1, 2)
      .map(x => x * 5)
      .take(4)
      .toVector should contain inOrder(5, 15, 25, 35)
  }

  //Functor = map
  //Applicative = None
  //Monad = flatMap

  test(
    """Map in an Option, although an Option is not a collection,
      |  it is has some of the same attributes like map that will operate
      |  with its internals. To apply a map to a None will just render
      |  a None""".stripMargin) {
    Some(10).map(x => x + 40) should be(Some(50))
    None.asInstanceOf[Option[Int]].map(x => x + 40) should be(None)
  }

  test(
    """We can also use a map on a scala Map, you have two choices,
      | either map which takes the Tuples, or mapValues which just
      | maps over the values.""".stripMargin) {
    val mapStructure = Map(1 -> "One", 2 -> "Two", 3 -> "Three")
    val result = mapStructure.map(t => t._1 * 100 -> (t._2 + " Hundred"))
    result should contain(100 -> "One Hundred")
  }

  test("foldLeft") {
    val result = List(1, 2, 3, 4, 5).foldLeft(1) { (total, next) =>
      println(s"total $total, next: $next")
      total * next
    }
    result should be(120)
  }

  test(
    """reduce will collapse all elements of a collection using a function.
      |  It will start the first element as the 'seed' or 'accumulation"""
      .stripMargin) {
    val result = List(1, 2, 3, 4, 5).reduce { (total, next) =>
      println(s"total $total, next: $next")
      total * next
    }
    result should be(120)
  }

  test(
    """flatMap will not only apply the given function on all elements of a Traversable,
      |  but all elements within the elements and flatten the results""") {
    pending
  }

  test( """flatMap of Options will filter out all Nones and Keep the Somes""") {
    val list = List(1, 2, 3, 4, 5)
    val result = list.flatMap(it => if (it % 2 == 0) Some(it) else None)
    result should be(List(2, 4))
  }

  test(
    """foreach will apply a function to all elements of a Traversable, but unlike
      | the map function, it will not return anything
      | since the return type is Unit, which
      | is like a void return type in Java, C++""".stripMargin) {
    pending
  }

  test(
    """groupBy will categorize a collection by a function, and return a
      |  map where the keys were derived by that function""".stripMargin) {
    val result = List("I see trees of green", "Red roses too",
      "I see them bloom",
      "for me and you")
      .flatMap(w => w.split(" "))
      .groupBy(w => w)
      .mapValues(v => v.size)
    result should contain("see" -> 2)
  }

  test(
    """mkString will create a string from a
      | collections elements, and offers
      | multiple ways to do so""".stripMargin) {
    val result = List("Foo", "Bar", "Baz").mkString("{", ",", "}")
    result should be("{Foo,Bar,Baz}")
  }

  test(
    """collect will apply a partial function to all elements
      |  and will return a different collection.""".stripMargin) {
    pending
  }

  test(
    """scan is like a reduce but maintains a running total
      |  with each iteration""".stripMargin) {
    pending
  }

  test("""zip will interweave two collections together leaving a tuple""") {
    val nums = List(1, 2, 3, 4)
    val chars = List('a', 'b', 'c')
    nums zip chars should contain inOrder((1, 'a'), (2, 'b'), (3, 'c'))
  }

  test(
    """view will not immediately evaluate a chain until a terminal
      |  operation is called, like reduce, count, or force""".stripMargin) {
    val result = (1 to 10000000).view.map(x => x * 4000).take(4).force.toList
    result should contain inOrder(4000,8000,12000,16000)
  }

  test(
    """sorted will sort the collection based on an implicit ordering
      |  and return that ordered collection""".stripMargin) {
    val sortedList = List("bassoon", "bass", "violin", "guitar", "cello").sorted
    sortedList should contain inOrder
      ("bass", "bassoon", "cello", "guitar", "violin")
  }

  test(
    """sortBy will also sort the collection based on an
      |  implicit rule, but will apply a function first""".stripMargin) {

    val names = List("Ella Fitzgerald",
      "Louis Armstrong",
      "Albert Einstein",
      "Tim Berners Lee",
      "Nikola Tesla",
      "Bob Marley").sortBy(s => s.split(" ").last)

    names.take(2) should contain inOrder("Louis Armstrong", "Albert Einstein")
  }
}
