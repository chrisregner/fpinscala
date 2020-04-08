// package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // If the mean of a sequence is m, the variance is
  // the mean of math.pow(x - m, 2) for each element x in the sequence.
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m =>
      mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
    case _ => None
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldLeft(Some(List()): Option[List[A]])((optOfL, opt) =>
      opt.flatMap(a =>
        optOfL.map(_ :+ a)))

  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldLeft(Some(List()): Option[List[B]])((optOfL, a) =>
        optOfL.flatMap(l =>
          f(a).map(l :+ _)))
}
