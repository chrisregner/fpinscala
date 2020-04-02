# Questions

- What is covariance; What is `+A` in
```scala
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
```
- How to use variadic function syntax:
```scala
def apply[A](as: A*): List[A] = // Variadic function syntax
  if (as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))
```
