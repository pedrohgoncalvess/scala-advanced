package others

object PatternMatching extends App{

  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"The only element is $head")
    case _ =>
  }

  //pattern matching class

  class Person(val name:String, val age:Int)

  object Person {
    def unapply(person:Person): Option[(String, Int)] = Some((person.name, person.age))
    def unapply(age: Int): Option[String] = Some(if (age < 18) "minor" else "major")
  }

  val pedro = new Person(name="Pedro", age=21)

  val greeting = pedro match {
    case Person(n, a) => s"Hi, my name is $n and I am $a yo."
  }

  val legalStatus = pedro.age match {
    case Person(status) => s"My legal status is $status"
  }


  object singleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  val number:Int = 8

  val matchProperty = number match {
    case singleDigit() => "single digit"
    case even() => "an even number"
    case _ => "no property"
  }


  case class Or[A, B](a: A, b: B)
  val either = Or(2,"two")
  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }

  println(humanDescription)

  val vararg = numbers match {
    case List(1, _*) => "starting with 1" //_* represents the rest of list
  }


  //pattern matching sequences

  abstract class MyList[+A] {
    def head:A = ???
    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))

  val decomposed = myList match {
    case MyList(1, 2, _*) => "start with 1 and 2"
    case _ => "something else"
  }

  println(decomposed)

  abstract class Wrapper[A] {
    def isEmpty: Boolean
    def get: A
  }

  object PersonWrapper {
    def unapply(person:Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty = false
      def get:String = person.name
    }
  }

  println(pedro match {
    case PersonWrapper(n) => s"This person's name is $n"
    case _ => "An alien"
  })
}