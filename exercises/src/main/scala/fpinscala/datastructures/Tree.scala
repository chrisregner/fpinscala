package fpinscala.datastructures

import scala.math.{max}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def toString(t: Tree[Int]): String = t match {
    case Leaf(a) => s"Leaf(${a.toString})"
    case Branch(l, r) => s"Branch(${toString(l)}, ${toString(r)})"
  }

  def fold[A, B](t: Tree[A], b: B)(f: (B, A) => B): B = t match {
    case Leaf(a) => f(b, a)
    case Branch(l, r) => fold(r, fold(l, b)(f))(f)
  }

  // def fold[A, B](t: Tree[A], b: B)(f: (B, A) => B): B = t match {
  //   case Leaf(a) => f(b, a)
  //   case Branch(l, r) => fold(r, fold(l, b)(f))(f)
  // }

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // def size[A](t: Tree[A]): Int =
  //   fold(t, 0)((b, _) => b + 1)

  def sizeTest(): Unit = {
    // test size[]
    val fnName = "size"
    val t =
      Branch(
        Branch(
          Leaf(true),
          Branch(
            Leaf(true),
            Leaf(true)
          )
        ),
        Leaf(true),
      )
    val expected = 7
    val actual = size(t)
    val status = if (actual == expected) "SUCCESS" else "FAILED"
    println(s"$status: $fnName() returned $actual, expected $expected")
  }

  def maximum(t: Tree[Int]): Int = {
    def go(max: Int, t: Tree[Int]): Int = t match {
      case Leaf(int) => if (int > max) int else max
      case Branch(l, r) => go(max, l) max go(max, r)
    }

    go(Int.MinValue, t)
  }

  def testMaximum1(): Unit = {
    // test size[]
    val fnName = "maximum"
    val t =
      Branch(
        Branch(
          Leaf(1),
          Branch(
            Leaf(4),
            Leaf(2)
          )
        ),
        Leaf(4),
      )
    val expected = 4
    val actual = maximum(t)
    val status = if (actual == expected) "SUCCESS" else "FAILED"
    println(s"$status: $fnName() returned $actual, expected $expected")
  }

  def testMaximum2(): Unit = {
    // test size[]
    val fnName = "maximum"
    val t =
      Branch(
        Branch(
          Leaf(-3),
          Branch(
            Leaf(-4),
            Leaf(-2)
          )
        ),
        Leaf(-1),
      )
    val expected = -1
    val actual = maximum(t)
    val status = if (actual == expected) "SUCCESS" else "FAILED"
    println(s"$status: $fnName() returned $actual, expected $expected")
  }

  def depth(t: Tree[Int]): Int = {
    def go(depth: Int, t: Tree[Int]): Int = t match {
      case Leaf(_) => depth + 1
      case Branch(l, r) => max(1 + go(depth, l), 1 + go(depth, r))
    }

    go(0, t)
  }

  def testDepth(): Unit = {
    // test size[]
    val fnName = "depth"
    val t =
      Branch(
        Branch(
          Leaf(1),
          Branch(
            Leaf(4),
            Leaf(2)
          )
        ),
        Leaf(4),
      )
    val expected = 4
    val actual = depth(t)
    val status = if (actual == expected) "SUCCESS" else "FAILED"
    println(s"$status: $fnName() returned $actual, expected $expected")
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def testMap(): Unit = {
    // test size[]
    val fnName = "map"
    val t =
      Branch(
        Branch(
          Leaf(1),
          Branch(
            Leaf(4),
            Leaf(2)
          )
        ),
        Leaf(4),
      )
    val expected =
      Branch(
        Branch(
          Leaf(10),
          Branch(
            Leaf(40),
            Leaf(20)
          )
        ),
        Leaf(40),
      ).toString
    val actual = map(t)(_ * 10).toString
    val status = if (actual == expected) "SUCCESS" else "FAILED"
    println(s"$status: $fnName() returned $actual, expected $expected")
  }

  def test(): Unit = {
    sizeTest()
    testMaximum1()
    testMaximum2()
    testDepth()
    testMap()
  }
}
