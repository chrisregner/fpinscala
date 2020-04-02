package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  val y = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  // def append2[A](a1: List[A], a2: List[A]): List[A] =
  //   foldLeft(a1, a2)((b, a) => Cons(a, b))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  //   foldLeft(as, ???))((b, a) => (g) => g(f(a, b)))

  def sum2(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(ns: List[Double]): Double =
    ns match {
      case Nil => 1.0
      case Cons(0, _) => 0
      case Cons(x, xs) => x * product3(xs)
    }

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }

  def tail2[A](l: List[A]): List[A] =
    drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case _ => Cons(h, l)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(a, as) if n > 0 => drop(as, n - 1)
      case _ => l
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(a, as) if f(a) => dropWhile(as)(f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =  {
    def loop(accA: List[A], remainingA: List[A]): List[A] = remainingA match {
      case Nil => accA
      case _ if (length(remainingA) == 1) => accA
      case Cons(x, xs) => loop(append(accA, List(x)), xs)
    }

    loop(Nil, l)
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, count) => count + 1)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((count, _) => count + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    def loop(as: List[A], bs: List[B]): List[B] =
      as match {
        case Nil => bs
        case Cons(h, t) => loop(t, Cons(f(h), bs))
      }

    loop(l, Nil)
  }

  def reverse[A](l: List[A]) =
    foldLeft(l, Nil: List[A])((as, a) => Cons(a, as))

  def flatten[A](lOfL: List[List[A]]): List[A] =
    foldLeft(lOfL, Nil: List[A])((acc, l) => append(acc, l))
}
