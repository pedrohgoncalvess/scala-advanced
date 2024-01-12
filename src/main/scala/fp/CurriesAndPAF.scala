package fp

@main def CurriesAndPAF: Unit = {

  val superAdder: Int => Int => Int = x => y => x + y   //curried function
  val add3 = superAdder(3)
  println(add3(5))

  def curriedAdder(x:Int)(y:Int):Int = x + y //curried method

  val add4: Int => Int /*need type annotation*/ = curriedAdder(4)

  def inc(x: Int) = x+1
  List(1,2,3).map(x => inc(x)) //ETA-expansion

  // Partial function applications
  val add5 = curriedAdder(5) _ //Int => Int

  val simpleAddFunction = (x: Int, y:Int) => x + y
  def simpleAddMethod(x:Int, y:Int) = x + y
  def curriedAddMethod(x: Int)(y:Int) = x + y

  val add7 = (x: Int) => simpleAddFunction(7, x)
  val add7_2 = simpleAddFunction.curried(7)
  val add7_3 = curriedAddMethod(7) _ //PAF
  val add7_4 = curriedAddMethod(7)(_) //PAF = alternative syntax

  val add7_5 = simpleAddMethod(7, _: Int) //alternative syntax for turning methods into function values
              // y => simpleAddMethod(7, y)

  //underscore are powerful
  def concatenator(a: String, b:String, c:String) = a + b + c

  val insertName: String => String /*dont need type annotation*/ = concatenator("Hello, I'm ", _: String, ", how are you?") // x:String => concatenator(hello, x, how are you)
  println(insertName("Pedro"))

  val fillInTheBlanks = concatenator("Hello, ", _:String, _:String) //(x, y) => concatenator(hello, x, y)
  println(fillInTheBlanks("Pedro", " Scala"))


  def curriedFormatter(f: String)(number: Double): String = f.format(number)
  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)


  val simpleFormat = curriedFormatter("%4.2f") _
  val seriousFormat = curriedFormatter("%8.6f") _
  val preciseFormat = curriedFormatter("%14.12f") _

  println(numbers.map(curriedFormatter("14.12f"))) //does curried function. curriedFormatter("14.12f") turns in curriedFormatter(number:Double) and map is hof which accept one parameter of type double


  def byName(n: => Int) = n + 1 //is basically def number:int => 2 | byName(number) return is 2 + 1
  def byFunction(f: () => Int) = f() +1 //is same of byName. f is a function which does not receive any parameters and return integer, the return of byFunction is f + 1

  def method: Int = 42 //just storage a value
  def parenMethod(): Int = 42

  byName(42) //ok
  byName(method) //ok
  byName(parenMethod()) //without parentheses also works
  //byName(() => 42) //not work
  byName((() => 42)()) // first () (parentheses) is a definition of a function and last () is a call, without last () doesnt work

  //byFunction(42) //not work this function expects a lambda function
  //byFunction(method) //not ok!!!! doest not do ETA-expansion
  byFunction(parenMethod) // ok compiler does ETA-expansion
  byFunction(() => 42) //works, this is a lambda function
  byFunction(parenMethod _) // also works


  val myFunction: (Int, Int) => Int = (x, y) => x + y

}
