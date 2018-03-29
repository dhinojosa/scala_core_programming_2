package com.ora.scalaprogrammingfundamentals

import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable.ArrayBuffer

class SealedTraitsSpec extends FunSuite with Matchers {
  test("A trait is analogous to an interface in Java") {
    trait Vehicle {
      def increaseSpeed(ms: Int): Vehicle

      def decreaseSpeed(ms: Int): Vehicle

      def currentSpeedMetersPerHour: Int
    }

    class Bicycle(val currentSpeedMetersPerHour: Int) extends Vehicle {
      override def increaseSpeed(ms: Int): Vehicle =
        new Bicycle(currentSpeedMetersPerHour + ms)

      override def decreaseSpeed(ms: Int): Vehicle =
        new Bicycle(currentSpeedMetersPerHour - ms)
    }

    new Bicycle(1)
      .increaseSpeed(3)
      .decreaseSpeed(1)
      .currentSpeedMetersPerHour should be(3)
  }

  test(
    """Just like Java 8 interfaces, you can have concrete
      |  methods (known as default methods in Java)""".stripMargin) {
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

  test("Traits are specifically called that just for mixing in functionality") {
    trait Log {
      private val _log: ArrayBuffer[String] = ArrayBuffer[String]()

      def log(s: String): Unit = _log += s

      def entries: ArrayBuffer[String] = _log
    }

    val o = new Object with Log
    o.log("Sent one statement")
    o.log("Sent two statements")

    o.entries should contain inOrder
      ("Sent one statement", "Sent two statements")
  }

  test(
    """A sealed trait is a trait that will have children,
      | but it will define all it's children and not one else will have the
      | ability to extend the number of children any further. All children
      | must be produced within the same file""".stripMargin) {
    pending
  }

  test(
    """A popular sealed trait is Option[+T], Some[T], None, let's
      | take a look at the API.""".stripMargin) {
    pending
  }

  test(
    """A popular sealed trait is Also List[A], ::,
      |and Nil let's take a look at the API.""".stripMargin) {
    pending
  }

  test(
    """It is important to note that the children of a sealed trait are
      |  subclassable, and would typically require a final modifier"""
      .stripMargin) {
    pending
  }

  test(
    """Sealed traits is also a good idea for pattern matching
      | exhaustiveness. The compiler will be able to recognize the subclasses
      | of all sealed traits""".stripMargin) {
    pending
  }

  test(
    """You can also have sealed abstract classes, which will operate under
      |  the same rules, the children must all be inside the same file,
      |  and the children should be final.""".stripMargin) {
    pending
  }
}
