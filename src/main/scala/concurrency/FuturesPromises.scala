package concurrency

import scala.concurrent.Future
import scala.util.{Failure, Success}

//important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesPromises extends App:
  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife // calculates the meaning of life on ANOTHER thread
  } // (global) which is passed by the compiler

  println(aFuture.value) //Option[Try[Int]]

  println("Waiting on the future")
  aFuture.onComplete { //works with pattern matching
    case Success(meaningOfLife) => println(s"The meaning of life is $meaningOfLife")
    case Failure(exception) => println(s"I have failed with $exception")
  } //SOME thread

  Thread.sleep(3000)

