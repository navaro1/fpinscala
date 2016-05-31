package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(t: Tree[_]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,  r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(v)      => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth(t: Tree[_]): Int =
    t match {
      case Leaf(_)      => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B =
    t match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold[T](t: Tree[T])(implicit ev: Numeric[T]): T = 
    fold(t)(a => a)(ev.max(_, _))

  def depthViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 0)((l, r) => 1 + (l max r))
}
