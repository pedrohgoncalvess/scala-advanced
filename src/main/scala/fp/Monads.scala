package fp

object Monads extends App {

  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  /*
  left-identity 1 law

  unit.flatMap(f) = f(x)
  Attempt(x).flatMap(f) = f(x) //Success case!
  Success(x).flatMap(f) = f(x) //proved.


  right-identity 2 law

  Attempt.flatMap(f).flatMap(g) == Attempt.flatMap(x => f(x).flatMap(x))
  Fail(x).flatMap(f).flatMap(g) = Fail(e)
  Fail(e).flatMap(x => f(x).flatMap(g)) = Fail(e)

  Success(v).flatMap(f).flatMap(g) =
    f(v).flatMap(g) or Fail(e)

  Success(v).flatMap(x => f(x).flatMap(g)) =
    f(v).flatMap(g) or Fail(e)
  */

  val attempt = Attempt {
    throw new RuntimeException("My own monad")
  }

  println(attempt)

  class Lazy[+A](value: => A) {
    def flatMap[B](f: A => Lazy[B]): Lazy[B] = f(value)
  }
  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyInstance = Lazy {
    println("Gato punheta churrasqueiro")
    17
  }

}
