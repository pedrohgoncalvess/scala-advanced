package others

import scala.concurrent.Future
import scala.util.{Failure, Success}

object Multithreading extends App{

  // creating thread in JVM

  val aThread = new Thread(() => println("I'm running a parallel"))
  aThread.start()
  aThread.join()

  val helloThread = new Thread(() => (1 to 1000).foreach(_ => println("Hello")))
  val goodByeThread = new Thread(() => (1 to 1000).foreach(_ => println("Good bye")))

  helloThread.start()
  goodByeThread.start()
  /*different runs produce different results*/

  class BankAccount(var mount:Int):
    override def toString: String = "" + mount

    def withdraw(money:Int) = this.mount -= money

    def safeWithDraw(money:Int) = this.mount.synchronized {
      this.mount -= money
    }

    /*
    BankAccount(10000)

    Thread1 = withdraw(1000)
    Thread2 = withdraw(2000)

    Thread1 -> this.amount = this.amount - .... //PREEMPTED BY THE OS
    Thread2 -> this.amount = this.amount - 2000 = 8000

    inter-thread communication on the JVM
    wait - notify mechanism
    */

    //SCALA FUTURES

    import scala.concurrent.ExecutionContext.Implicits.global
    val aFuture: Future[Int] = Future {
      //long computation on a different thread
      42
    }

    aFuture.onComplete{
      case Success(42) => println("I found the meaning of life.")
      case Failure(_) => println("I not found the meaning of life.")
    }

    val aProcessedFuture = aFuture.map(_ + 1) //aFuture with 43
    val aFlatFuture = aFuture.flatMap{ value =>
      Future(value + 2)
    } //aFuture with 44

    val filteredFuture = aFuture.filter(_ % 2 == 0) //NoSuchElementException

    //for comprehension
    val aNonsenseFuture = for {
      meaningOfLife <- aFuture
      filteredMeaning <- filteredFuture
    } yield meaningOfLife + filteredMeaning


}
