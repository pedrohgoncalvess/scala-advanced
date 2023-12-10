package fp

object AnonymousFunctions extends App{

  val incrementer = new Function1[Int, Int] {
    def apply(v1: Int) = v1 + 1
  }

  val incrementer2 = (x:Int) => x*2

  val incrementer3: Int => Int = (x) => x * 3

  println(List(1,2,3).map(incrementer))
  println(List(1,2,3).map(incrementer2))
  println(List(1,2,3).map(incrementer3))

}
