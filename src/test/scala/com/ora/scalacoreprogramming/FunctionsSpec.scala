package com.ora.scalacoreprogramming

import org.scalatest.{FunSuite, Matchers}

class FunctionsSpec extends FunSuite with Matchers {
  test(
    """As a reminder from all those that took the beginner's course,
      |  a function really is an anonymous
      |  instantiation of a trait.""".stripMargin) {
    
    val f1:String => Int = new Function1[String, Int] {
      override def apply(v1: String): Int = v1.length
    }

    def foo(s:String) = s.length
    val f15: String => Int = foo _

    f1("Hello") should be(5)
  }

  test("The above can be whittled down to the following:") {
    val f = (s: String) => s.length
    f("Zanzibar") should be(8)
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
    f("Andorra") should be(7)
  }

  test(
    """If the type system has enough information either because
      |  of the left hand side of an assignment, or a parameter in method, or
      |  the way generic types are situated within a class,
      |  you can get rid of some additional code. In the following example, you
      |  can drop the underline and leave 5+, but will come with some warnings
      |  that you can turn off with """.stripMargin) {

    import scala.language.postfixOps
    val f: Int => Int = (5+)
    f(3) should be(8)
  }

  test(
    """A closure is a function that will "wrap" or "close"
      |  around an outside value.""".stripMargin) {
    def createFunction(i: Int): Int => Int = {
      (x: Int) => x + i
    }

    val add5Function: Int => Int = createFunction(5)
    val add10Function: Int => Int = createFunction(10)

    Vector(1, 2, 3).map(add5Function) should contain inOrder(6, 7, 8)
    Vector(1, 2, 3).map(add10Function) should contain inOrder(11, 12, 13)
  }

  test("""Lab: Create a method called lessThan that accepts an integer, we'll call it x,
      |  and returns a function that given another Int, we'll call it y,
      |  determines if y is less than x.  Now. Create two functions or methods called
      |  isFreezingCelcius and isFreezingFahrenheit that takes an int and uses
      |  lessThan to determine if a given Int temperature is freezing.
      |  Celcius freezing temperature is 0, Fahrenheit is 32.""".stripMargin) {

    pending

    def lessThan(x:Int):Int => Boolean = ???
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

    val manuallyCurried: Int => Int => Int => Int =
         (x: Int) => (y: Int) => (z: Int) => x + y + z

    manuallyCurried(3)(4)(10) should be(17)
  }

  test(
    """Compose is the equivalent of f(g(x)).  But what this is takes
      |  some perspective.  g(x) should be evaluated first and then the
      |  result of that (let's call it a) should be applied to f,
      |  therefore f(a) to get result b.  But these functions can be
      |  applied together to form one cohesive function""".stripMargin) {


    //a = g(x)
    //b = f(g(x)) = f(a)

    val tupleFirst: ((String, Int)) => String = (t: (String, Int)) => t._1
    val getFirstThreeLetters = (s: String) => s.substring(0, 3)

    val newFunction: ((String, Int)) => String = getFirstThreeLetters.compose(tupleFirst)

    newFunction(("Arizona", 3)) should be ("Ari")
  }

  test(
    """andThen is g(f(x)). f(x) is applied first and
      |  then g is then applied. In the following example we
      |  recreate the compose but using andThen""".stripMargin) {

    val tupleFirst = (t: (String, Int)) => t._1
    val getFirstThreeLetters = (s: String) => s.substring(0, 3)

    val newFunction: ((String, Int)) => String =
      tupleFirst.andThen(getFirstThreeLetters)
  }


  test("""Map will apply the given function on all elements of a
      |  Traversable and return a new collection
      |  of the result.""".stripMargin) {
    import scala.language.postfixOps
    val vector = Vector(1, 3, 4, 6)
    val result = vector.map(4*)
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
    val mapStructure = Map(1 -> "One", 2 -> "Two", 3 -> "Three")
    val result = mapStructure.map(kv => (kv._1 * 100, kv._2 + " Hundred"))
    result should contain (100 -> "One Hundred")
  }

  test("""filter will choose the ones that meet a predicate""") {
    List(1,2,3).filter(x => x % 2 != 0) should be (List(1,3))
  }

  test("""flatMap will not only apply the given function on all
      |  elements of a Traversable,
      |  but all elements within the elements
      |  and flatten the results""".stripMargin) {

    val xs: List[Int] = List(1,2,3,4,5).map(x => x + 3)
    val xs2: List[Int] = List(1,2,3).flatMap(x => List(-x, x, x+1))
    xs2 should be (List(-1,1,2,-2,2,3,-3,3,4))
  }

  test(
    """flatMap is great for big data analysis, word count.
      |  Here we will also use groupBy. GroupBy will categorize
      |  a collection by a function, and return a
      |  map where the keys were derived by that function""".stripMargin) {

    val lyrics = List("I see trees of green",
                      "Red roses too",
                      "I see them bloom",
                      "For me and you",
                      "and I think to myself",
                      "What a wonderful world")

    val stringses: Seq[String] = lyrics.flatMap(str => str.split(" "))
    val map = stringses.map(s => s.toLowerCase).groupBy(s => s.head)
    val maybeSeq: Option[Seq[String]] = map.get('s')
    maybeSeq should be (Some(Seq("see", "see")))
  }

  test("""flatMap also wonderful for digging
      |  into one-to-many object graphs""".stripMargin) {
    class Employee(val firstName:String, val lastName:String)
    class Manager(firstName:String, lastName:String, val employees:List[Employee])
      extends Employee(firstName, lastName)

    val employees1 = List[Employee](new Employee("Simon", "Simons"),
                          new Employee("Roger", "Japan"))

    val employees2 = List[Employee](new Employee("Anne", "Norway"),
                          new Employee("Yasmina", "Greco"),
                          new Employee("Carlos", "Canada"))

    val manager1 = new Manager("Bjarne", "Strousoup", employees1)
    val manager2 = new Manager("Grace", "Hopper", employees2)

    val result: Seq[Employee] = List(manager1, manager2).flatMap(ma => ma.employees)
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
    List(1,2,3) map println
  }


  test("""mkString will create a string from a
      |  collections elements, and offers
      |  multiple ways to do so""".stripMargin) {
    List(1,2,3).mkString(":") should be ("1:2:3")
    List(1,2,3).mkString("<", ":", ">") should be ("<1:2:3>")
  }

  test("""collect will apply a partial function to all elements
          |  and will return a different collection. It is a
          |  combo of map and filter""".stripMargin) {

    val doubleEvens = new PartialFunction[Int, Int] {
      override def isDefinedAt(x: Int): Boolean = x % 2 == 0

      override def apply(v1: Int): Int = v1 * 2
    }

    val doubleEvens2:PartialFunction[Int, Int] = {case x if x % 2 == 0 => x * 2}
    val tripleOdds:PartialFunction[Int, Int] = {case x if x % 2 != 0 => x * 3}

    List(1,2,3).collect{case x if x % 2 == 0 => x * 2} should be (List(4))
    List(1,2,3).collect(doubleEvens) should be (List(4))
  }

  test("""scan is like a reduce but maintains a running total
      |  with each iteration""".stripMargin) {
    val result = List(1,2,3,4).scan(0){(total, next) =>
      println(s"total: $total, next: $next")
      total + next}

    result should be (List(0, 1, 3, 6, 10))
  }

  test("""foldLeft will take two parameters group, the first
         |  will contain a seed and then a function that will
         |  aggregate the collection into one.""".stripMargin) {
    val result = List(1,2,3,4).foldLeft(0){(total, next) =>
      println(s"total: $total, next: $next")
      total + next}

    result should be (10)
  }

  test("""foldRight will take two parameters group, the first
         |  will contain a seed and then a function that will
         |  aggregate the collection into one, but do so coming
         |  in from the right hand side.""".stripMargin) {
    val result = List(1,2,3,4).foldRight(0){(next, total) =>
      println(s"total: $total, next: $next")
      total + next}

    result should be (10)
  }

  test("""reduce will collapse all elements of a collection using a function.
         |  It will start the first element as the 'seed' or 'accumulation"""
    .stripMargin) {
    val result = List(1,2,3,4).reduce{(total, next) =>
        println(s"total: $total, next: $next");
        total + next}

    result should be (10)
  }

  test("""zip will interweave two collections together leaving a tuple""") {
    val result = List(1,2,3).zip(List('a', 'b', 'c'))
    result should be(List((1,'a'), (2,'b'), (3,'c')))
  }

  test("""view will not immediately evaluate a chain until a terminal
      |  operation is called, like reduce, count, or force""".stripMargin) {
    val result = (1 to 10000000)
      .view
      .map{i => println(i);i * 10}
      .take(3)
      .toList //terminal operation

    result should be (List(10, 20, 30))
  }

  test("""sorted will sort the collection based on an implicit ordering
      |  and return that ordered collection""".stripMargin) {
    val xs = List("One", "Two", "Three", "Four").sorted
    xs should be (List("Four", "One", "Three", "Two"))
  }

  test("""sortBy will also sort the collection based on an
      |  implicit rule, but will apply a function first""".stripMargin) {
    val result = List((1,"Foo"), (2, "Bar"), (3, "Baz")).sortBy(t => t._2)
    result should be (List((2, "Bar"), (3, "Baz"), (1, "Foo")))
  }

  test("""Lab: Functional challenge. Using the fuctions we covered, write
      |  factorial functionally. No loops, no recursion. Hint: Most of the
      |  methods in BigInt are the same as Int so you can keep it simple.
      |  """.stripMargin) {

    pending

    def factorial(x:BigInt):BigInt = ???

    Stream.from(1)
          .map(x => factorial(x))
          .take(5)
          .toList shouldBe inOrder (1,2,6,24,120,720)
  }

  test("""Advanced Lab (If time remains): create a grid. If provided a size,
      |   create a grid with (size x size) dimensions. See the test for what is
      |   expected. You will need to look through the API for ways to do this.
      |   There is a method we haven't covered yet. If you give up,
      |   the answer is available here, but try your best:
      |   https://bit.ly/2zxPS9k """.stripMargin) {

    pending

    def grid(size:Int):String = ???

    grid(2) should be (
      """1,2
        |3,4""".stripMargin)

    grid(3) should be (
      """1,2,3
        |4,5,6
        |7,8,9""".stripMargin
    )

    grid(4) should be (
      """01,02,03,04
        |05,06,07,08
        |09,10,11,12
        |13,14,15,16""".stripMargin
    )
  }

  test("""Super Advanced Lab: No way you are going to solve this during class.
      |  Given the following cube:
      |
      |  TODD
      |  OMAR
      |  DAVE
      |  DREW
      |
      |  Notice that TODD is across in the first row and down in the first
      |  column. Notice how OMAR is across on 2nd row and down on 2nd column
      |  Notice the same for DAVE and DREW. Find the female equivalent cube that
      |  works the same way. Find a list of female names on the web. Use
      |  functional programming, NOT imperative programming.""".stripMargin) {
    pending
  }
}
