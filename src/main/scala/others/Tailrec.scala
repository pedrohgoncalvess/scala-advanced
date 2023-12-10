package others

import scala.annotation.tailrec

object Tailrec extends App{

  @tailrec def factorial(n:Int, accumulator: Int): Int = {
    if (n < 0) 1
    else factorial(n - 1, n * accumulator)
  }

}
