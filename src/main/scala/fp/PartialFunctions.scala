package fp

object PartialFunctions extends App{

    val aFussyFunction = (x:Int) =>
      if (x==1) 42
      else if (x==2) 56
      else if (x==5) 999
      else throw new FunctionNotApplicableException

    class FunctionNotApplicableException extends RuntimeException

    val aNicerFussyFunction = (x: Int) => x match {
      case 1 => 42
      case 2 => 56
      case 5 => 999
    }

    val aPartialFunction: PartialFunction[Int, Int] = {
      case 1 => 42
      case 2 => 56
      case 5 => 999
    } //if parameter does not match, an exception will be thrown

   println(aPartialFunction(2))
   //println(aPartialFunction(3) //throws a exception

    val functionLifted = aPartialFunction.lift //encapsulates the return in a Some()
    println(functionLifted(1))
    println(functionLifted(3))

    val aMappedList = List(1,2,3).map { //HOFs accept partial functions as well
      case 1 => 42
      case 2 => 78
      case 3 => 1000
    }
}
