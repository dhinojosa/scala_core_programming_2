package com.ora.scalaprogrammingfundamentals

import org.scalatest.{FunSuite, Matchers}

class FunctionsSpec extends FunSuite with Matchers {
  test(
    """As a reminder from all those that took the beginner's course,
      |  a function really is an anonymous
      |  instantiation of a trait.""".stripMargin) {

    val f = new Function[String, Int] {
      override def apply(v1: String): Int = v1.length
    }

    f.apply("Hello") should be(5)
  }

  test("The above can be whittled down to the following:") {
    val f = (s: String) => s.length
    f.apply("Zanzibar") should be (8)
  }

  test(
    """If you declare the left hand side you can
      |  do some nifty tricks on the right hand side:""".stripMargin) {
    val f: String => Int = s => s.length
    f.apply("Andorra") should be (7)
  }


  test(
    """Also, since the left hand side, has all the type information,
      |  on the right hand side you can trim the left hand side
      |  with syntactical tricks like use the placeholder""".stripMargin) {
    val f: String => Int = _.length
    f.apply("Andorra") should be (7)
  }

  test(
    """If the type system has enough information either because
      |  of the left hand side of an assignment, or a parameter in method, or
      |  the way generic types are situated within a class,
      |  you can get rid of some additional code. In the following example, you
      |  can drop the underline and leave 5+, but will come with some warnings
      |  that you can turn off with """.stripMargin) {

    val f: Int => Int = 5 + _
    f.apply(3) should be (8)
  }


}
