package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(x, xs) => x() :: xs().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(x, xs) if n > 1 => cons(x(), xs().take(n - 1))
    case Cons(x, xs) if n == 1 => cons(x(), empty)
    case _ => Empty
  }

  def takeViaUnfold(n: Int): Stream[A] = 
    unfold((this, n)) {
      case (Cons(x, xs), n) if n > 1 => Option((x(), (xs(), n - 1)))
      case (Cons(x, _), n) if n == 1 => Option((x(), (empty, n - 1)))
      case _ => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(x, xs) if n > 0 => xs().drop(n - 1)
    case _ => this 
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(x, xs) if (p(x())) => cons(x(), xs().takeWhile(p))
    case _ => Empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = 
    unfold(this) {
    case Cons(x, xs) if p(x()) => Option((x(), xs()))
    case _ => None
    }

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((e, acc) => p(e) && acc)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = 
    foldRight(Empty: Stream[A])((e, acc) => if (p(e)) cons(e, acc) else empty)

  def headOption: Option[A] = 
    foldRight(None: Option[A])((e, _) => Option(e))

  def map[B](f: A => B): Stream[B] = 
    foldRight(Empty: Stream[B])((e, acc) => cons(f(e), acc))

  def mapViaUnfold[B](f: A => B): Stream[B] = 
    unfold(this) {
    case Cons(x, xs) => Option(f(x()), xs())
    case _ => None
    }

  def filter(p: A => Boolean): Stream[A] = 
    foldRight(Empty: Stream[A])((e, acc) => if (p(e)) cons(e, acc) else acc)

  def append[B>:A](other: => Stream[B]): Stream[B] = 
    foldRight(other)((e, acc) => cons(e, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(Empty: Stream[B])((e, acc) => f(e).append(acc))

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s2)) {
    case (_, Empty) => None
    case (Empty, _) => None
    case (Cons(x, xs), Cons(y, ys)) => Option(f(x(), y()), (xs(), ys()))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Cons(x, xs), Empty) => Some((Some(x()), None) -> (xs(), Empty))
    case (Empty, Cons(y, ys)) => Some((None, Some(y())) -> (Empty, ys()))
    case (Cons(x, xs), Cons(y, ys)) => Some((Some(x()), Some(y())) -> (xs(), ys()))
  }

  def startsWith[B](s: Stream[B]): Boolean = 
    this.zipAll(s).takeWhile(!_._2.isEmpty).forAll {
    case (x, y) => x == y
  }

  def tails: Stream[Stream[A]] =
    unfold(this) {
    case Empty => None
    case xs => Some((xs, xs drop 1))
  }.append(Stream(empty))

  def scanRight[B](s: B)(f: (A, B) => B): Stream[B] = 
    foldRight((s, Stream(s)))((e, acc) => {
      lazy val h = f(e, acc._1)
      (h, cons(h, acc._2))
    })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = 
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = 
    Stream.cons(n, from(n+1))

  lazy val fibs: Stream[Int] = {
      def fibsLoop(a: Int, b: Int): Stream[Int] =
        cons(a, fibsLoop(b, a + b))
      fibsLoop(0, 1)
    }
    

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }

  lazy val fibsViaUnfold: Stream[Int] = 
    unfold((0, 1)){ case (a, b) => Option(a, (b, a + b)) }

  def fromViaUnfold(n: Int): Stream[Int] = 
    unfold(n)(a => Option(a, a + 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(a => Option(a, a))

  lazy val onesViaUnfold: Stream[Int] = 
  constantViaUnfold(1)
}