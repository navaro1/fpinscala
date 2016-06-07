package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (if (n < 0) -(n + 1) else n, r)    
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r0) = rng.nextInt
    val (d, r1) = double(r0)
    ((i, d), r1)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (id, r) = intDouble(rng)
    (id.swap, r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d0, r0) = double(rng)
    val (d1, r1) = double(r0)
    val (d2, r2) = double(r1)
    ((d0, d1, d2), r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def intsLoop(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (c > 0) {
        val (i0, r0) = r.nextInt
        intsLoop(c - 1, r0, i0 :: acc)
      } else
        (acc, r)
    }
    intsLoop(count, rng, Nil)
  }

  def doubleViaMap: Rand[Double] = 
    map(nonNegativeInt)(i => i / (Int.MaxValue + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, r0) = ra(rng)
      val (b, r1) = rb(r0)
      (f(a, b), r1)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
     fs.foldRight(unit(Nil: List[A]))((e, acc) => map2(e, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, r0) = f(rng)
      g(a)(r0)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = 
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n -1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = 
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}

import State._
case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.flatMap(b => unit(f(a, b))))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s0 => {
      val (a, s1) = run(s0)
      f(a).run(s1)
    })
      
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = 
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = 
    sas.foldRight(unit[S, List[A]](Nil))((e, acc) => e.map2(acc)(_ :: _))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def updateMachine(coins: Int, candies: Int, locked: Boolean): ((Int, Int), Machine) =
      ((coins, candies), Machine(locked, candies, coins))

    State(m => inputs.foldLeft(((m.coins, m.candies), m)){(acc, in) =>
      lazy val Machine(isMachineLocked, candies, coins) = acc._2
      in match {
        case _ if candies == 0 => acc
        case Coin if isMachineLocked => updateMachine(coins+1, candies, !isMachineLocked)
        case Turn if !isMachineLocked => updateMachine(coins, candies-1, !isMachineLocked)
        case _ => acc
      }
    })
  }
}