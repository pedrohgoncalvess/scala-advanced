package fp

import scala.annotation.tailrec

abstract class MyStream[+A] {
  def isEmpty: Boolean

  def head: A

  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B]

  def ++[B >: A](anotherStream: MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit

  def map[B](f: A => B): MyStream[B]

  def flatMap[B](f: A => MyStream[B]): MyStream[B]

  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A]

  @tailrec
  final def toList[B >: A](acc:List[B]=Nil): List[B] =
    if(isEmpty) acc.reverse
    else tail.toList(head :: acc)

    /*
    [1 2 3].toList([] <- acc)
    [2 3].toList([1])
    [] <- isEmpty .toList([3 2 1] <- reverse) = [1 2 3]
    */
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException

  def tail: MyStream[Nothing] = throw new NoSuchElementException

  def #::[B >: Nothing](element: B): MyStream[B] = Cons(element, this)

  def ++[B >: Nothing](anotherStream: MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = this

  def map[B](f: Nothing => B): MyStream[B] = this

  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  def take(n: Int): MyStream[Nothing] = this

}

class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false

  override val head: A = hd

  override lazy val tail: MyStream[A] = tl

  def #::[B >: A](element: B): MyStream[B] = new Cons(element, this)

  def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream)

  def foreach(f: A => Unit): Unit =
    f(head)
    tail.foreach(f)

  def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f)) //preserves lazy evaluation
  /*
  s = new Cons(1, ?)
  mapped = s.map(_ + 1) = new Cons(2, s.tail.map(_ + 1)) ... mapped.tail
  */

  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) new Cons(head, tail.filter(predicate))
    else tail.filter(predicate)

  def take(n: Int): MyStream[A] =
    if (n <= 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n-1))

}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = new Cons(start, MyStream.from(generator(start))(generator))
}

object Streams extends App{

  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0#:: naturals // naturals.#::(0)
  println(naturals.head)

  startFrom0.take(10000).foreach(println)
  println(startFrom0.map(_ * 2).take(100).toList())
}
