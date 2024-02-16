package fp

object LazyEvaluation extends App {

  lazy val x = {
    println("Boo")
    42
  }
  println(x) //output boo and 42
  println(x) //output 42 because lazy evaluation "executes" the variable once

  def lazyEval: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  println(if (simpleCondition && lazyEval) "yes" else "no") //output is just no because one condition is not true them lazy is not computed

  def byNameMethod(n: => Int): Int = {n + n + n + 2}
  def retrieveMagicValue: Int = {
    println("waiting")
    Thread.sleep(1000)
    42
  }
  println(byNameMethod(retrieveMagicValue)) //output is three times waiting is this case

  def callByNeed(n: => Int): Int = {
    lazy val t = n //evaluated once
    t + t + t + 2
  }
  println(callByNeed(retrieveMagicValue)) //output is one time waiting because lazy evaluation


  //lazy eval in filter
  def isGreaterThan20(n:Int):Boolean = {
    println("Is greater than 20?")
    n > 20
  }

  def isGreaterThan30(n:Int): Boolean = {
    println("Is less than 30?")
    n < 30
  }

  val numbers = List(1,60,21,12,32,28,24)

  val gt20 = numbers.filter(isGreaterThan20)
  val ls30 = gt20.filter(isGreaterThan30)
  println(ls30)

  val gt20lazy = numbers.withFilter(isGreaterThan20)
  val ls30lazy = gt20lazy.withFilter(isGreaterThan30)
  println(ls30lazy) //space allocated in memory
  ls30lazy.foreach(println) //shows the evaluation


  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head:A
    def tail: MyStream[A]

    def #::[B >: A](element:B): MyStream[B]
    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B]

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A]
    def takeAsList(n:Int): List[A]
  }

}
