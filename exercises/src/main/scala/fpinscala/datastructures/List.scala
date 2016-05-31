package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else l
  }

  def init[A](l: List[A]): List[A] =
    l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
    
  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((_, acc) => 1 + acc)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumViaFoldLeft(nums: List[Int]): Int = 
    foldLeft(nums, 0)((acc, n) => n + acc)

  def productViaFoldLeft(nums: List[Double]): Double = 
    foldLeft(nums, 1.0)((acc, n) => n * acc)

  def lengthViaFoldLeft(l: List[_]): Int = 
    foldLeft(l, 0)((acc, _) => 1 + acc)

  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, Nil: List[A])((acc, e) => Cons(e, acc))

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] = 
    foldRight(l1, l2)((e, acc) => Cons(e, acc))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = 
    foldLeft(reverse(a1), a2)((acc, e) => Cons(e, acc))

  def concat[A](l: List[List[A]]): List[A] = 
    foldRight(l, Nil: List[A])(append)

  def add1(nums: List[Int]): List[Int] = 
    foldRight(nums, Nil: List[Int])((e, acc) => Cons(e + 1, acc))

  def doubleToString(l: List[Double]): List[String] = 
    foldRight(l, Nil: List[String])((e, acc) => Cons(e.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((e, acc) => Cons(f(e), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldRight(l, Nil: List[A])((e, acc) => if (f(e)) Cons(e, acc) else acc)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = 
    foldRight(l, Nil: List[B])((e, acc) => append(f(e), acc))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = 
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
  }
    
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
  }

  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def startsWith(l: List[A], pre: List[A]): Boolean = (l, pre) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
      case _ => false
    }

    l match {
      case Nil => false
      case Cons(x, xs) if startsWith(Cons(x, xs), sub) => true
      case Cons(x, xs) => hasSubsequence(xs, sub)
    }
  }
}
