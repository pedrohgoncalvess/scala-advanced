package others

import scala.util.Try

object SyntaxSugar extends App{

  //syntax sugar 1: method with single param
  def singleArgMethod(arg:Int): String = s"$arg little ducks..."

  val description = singleArgMethod {
      //write some complex code
    42
  }

  val aTryInstance = Try {
    //write some complex code
    throw new RuntimeException
  }

  //syntax sugar 2: single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x:Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x:Int) => x + 1

  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Hello, Scala")
  })

  val aSweeterThread = new Thread(() => println("Sweet, Scala"))

  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a:Int):Unit
  }

  val anAbstractInstance: AnAbstractType = (a:Int) => println("Sweet")

  val prependedList = 8::2::List(2,3) //append in start of list
  val prependedList2 = 1 :: 2 :: 3 :: List(4,5) //ordered list


  class MyStream[T] {
    def -->:(value:T): MyStream[T] = this
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]
  println(myStream.-->:(2))

  class PersonSaid(name:String) {
    def `and then said`(message:String): Unit = println(s"$name said $message")
  }

  //syntax sugar 4 multi word method naming
  val pedro = new PersonSaid("Pedro")
  pedro `and then said` "Scala é foda meu patrao"

  //updating array
  val anArray = Array(1,2,3)
  anArray.update(2,7)

  class Mutable {
    private var internalMember: Int = 0
    def member:Int = internalMember //getter
    def member_=(value:Int):Unit = {
      internalMember = value //setter
    }
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 20
}
