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
    private val instanceValue = value //evaluate once time
    def use: A = instanceValue
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(instanceValue) // (=> A) <- this chain lazy builds. without this operations with flatMap are computed normally
  }
  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyInstance = Lazy {
    println("Gato punheta churrasqueiro")
    17
  }

  val flatMappedInstance = lazyInstance.flatMap( x => //doesnt print because the computation is lazy
    Lazy {
      10 * x
    }
  )

  val flatMappedInstance2 = lazyInstance.flatMap(x => //doesnt print because the computation is lazy
    Lazy {
      10 * x
    }
  )

  println(flatMappedInstance.use)
  println(flatMappedInstance2.use) //not print "Gato punheta churrasqueiro" because instanceValue is evaluated once time


  /*
  left-identity
  unit.flatMap(f) = f(v)
  Lazy(v).flatMap(f) = f(v)


  right-identity
  l.flatMap(unit) = 1
  Lazy(v).flatMap(x => Lazy(x)) = Lazy(v)


  associativity: l.flatMap(f).flatMap(g) = l.flatMap(x => f(x).flatMap(g))
  Lazy(v).flatMap(f).flatMap(g) = f(v).flatMap(g)
  Lazy(v).flatMap(x => f(x).flatMap(g)) = f(v).flatMap(g)
  */

  trait Monad[T] { //List
    def flatMap[B](f: T => Monad[B]): Monad[B]
    //def map[B](f: T => B): Monad[B] = flatMap(x => unit(f(x)))
    def flatten(m: Monad[Monad[T]]): Monad[T] = m.flatMap((x: Monad[T]) => x)

    //List(1,2,3).map(_ * 2) = List(1,2,3).flatMap(x => List(x * 2))
    //List(List(1,2),List(3,4)).flatten = List(List(1,2),List(3,4)).flatMap(x => x)
  }
}
