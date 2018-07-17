package com.ora.scalaprogrammingfundamentals

sealed trait Tree[+A]
final case class Node[A](left:Tree[A], right:Tree[A]) extends Tree[A]
final case class Leaf[A](value:A) extends Tree[A]
case object Empty extends Tree[Nothing]