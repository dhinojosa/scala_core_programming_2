package com.ora.scalaprogrammingfundamentals

import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable.ArrayBuffer

class SealedTraitsSpec extends FunSuite with Matchers {
  test("""A trait is analogous to an interface in Java, any method with
      |  no concrete implementation is considered abstract. Any subsequent
      |  inheritance of a trait is added using the keyword \"with\""""
      .stripMargin) {

    trait Vehicle {
      def increaseSpeed(ms: Int): Vehicle
      def decreaseSpeed(ms: Int): Vehicle
      def currentSpeedMetersPerHour: Int
    }

    trait Fun
    trait FreshAir

    case class Bicycle(currentSpeedMetersPerHour: Int) extends Vehicle with Fun with FreshAir {
      override def increaseSpeed(ms: Int): Vehicle =
        this.copy(currentSpeedMetersPerHour + ms)

      override def decreaseSpeed(ms: Int): Vehicle =
        this.copy(currentSpeedMetersPerHour - ms)
    }

    Bicycle(1)
      .increaseSpeed(3)
      .decreaseSpeed(1)
      .currentSpeedMetersPerHour should be(3) //should is a ScalaTest
  }

  test("""Just like Java 8 interfaces, you can have concrete
      |  methods (known as default methods in Java). In the
      |  example below: currentSpeedMilesPerHour""".stripMargin) {

    trait Vehicle {
      def increaseSpeed(ms: Int): Vehicle
      def decreaseSpeed(ms: Int): Vehicle
      def currentSpeedMetersPerHour: Int

      final def currentSpeedMilesPerHour: Double =
        currentSpeedMetersPerHour * 0.000621371
    }

    class Bicycle(val currentSpeedMetersPerHour: Int) extends Vehicle {
      override def increaseSpeed(mh: Int): Vehicle =
        new Bicycle(currentSpeedMetersPerHour + mh)

      override def decreaseSpeed(mh: Int): Vehicle =
        new Bicycle(currentSpeedMetersPerHour - mh)
    }

    new Bicycle(4).currentSpeedMilesPerHour should be(0.002 +- .005)
  }

  test("""Traits are specifically called that just for mixing in functionality""") {
    trait Log {
      private val _log: ArrayBuffer[String] = ArrayBuffer[String]()

      def log(s: String): Unit = _log += s

      def entries: List[String] = _log.toList
    }

    val o = new Object with Log
    o.log("Sent one statement")
    o.log("Sent two statements")

    //should contain inOrder is ScalaTest, not Scala core

    o.entries should contain inOrder ("Sent one statement", "Sent two statements")

    class Foo extends Log {
      def bar:Int = 3
    }

    val f = new Foo
    f.log("Hello")
    f.log("World")

    f.entries should contain inOrder("Hello", "World")
  }

  test("""A sealed trait is a trait that will have children,
      |  but it will define all it's children and not one else will have the
      |  ability to extend the number of children any further. All children
      |  must be produced within the same file. This will also create what
      |  is called a union type if you are familiar with Haskell, Elm, F#,
      |  and other functional languages. Let us create Node, Leaf,
      |  and Empty""".stripMargin) {
    val tree:Node[Int] = Node(Leaf(4), Node(Leaf(10), Leaf(40)))
    tree.left.asInstanceOf[Leaf[_]].value should be (4)
  }

  test(
    """You can also have sealed abstract classes, which will operate under
      |  the same rules, the subclasses must all be inside the same file,
      |  and the subclasses should be final. Why would you choose
      |  one over the other? You can multiple inherit a trait and mixin traits.
      |  Abstract classes offer stronger "is-a" relationships
      |  A popular sealed abstract class is Option[+T], Some[T], None, let's
      |  take a look at the API and try it out""".stripMargin) {
    val middleName:Option[String] = Some("Diego")
    middleName.getOrElse("No Middle Name") should be ("Diego")

    val danMiddleName:Option[String] = None
    danMiddleName.getOrElse("No Middle Name") should be ("No Middle Name")

  }

  test("""A popular sealed abstract class is Also List[A], ::,
      |  and Nil let's take a look at the API.""".stripMargin) {

    //Union Type
    val weirdWayOfCreatingAList  = new ::(1, List(1,2,3,4))
    val emptyList:List[Int] = Nil
    val oneElement = new ::(1,Nil)

    List.apply(1,2,3,4)
  }

  test("""Sealed traits are also a good idea for pattern matching
      |  exhaustiveness. The compiler will be able to recognize the subclasses
      |  of all sealed traits.""".stripMargin) {
    val a:Tree[Int] = Node(Leaf(3), Leaf(5))

    val result = a match {
      case Empty => "Empty Tree"
      case Leaf(x) => s"Leaf containing: $x"
      case Node(Leaf(x), Leaf(y)) => s"Node with two leaves with values $x, $y"
      case Node(x, y) => s"Node of $x, $y"
    }

    result should be ("Node with two leaves with values 3, 5")
  }
}
