package others

object Exceptions extends App{

  //val throwException = throw new RuntimeException //throws an exception at runtime

  val aPotentialFailure = try {
    throw new RuntimeException //Finalize the try block
    println("A runtime exception") //subsequents commands are not executed
  } catch {
    case e: Exception => "I caught an exception" //returns this string
  } finally {
    println("Some logs") //side effect in last block of try catch
  }

  println(aPotentialFailure)
}
