package fp

import scala.annotation.tailrec


/*
a trait in scala is quite similar to interfaces in other languages,
in our case we are doing the abstract implementation of some very famous methods in collections in
all programming languages so that subsequent classes make use of them adapting to their context such as NonEmptySet and EmptySet
*/
trait MySet[A] extends (A => Boolean) {
  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  def -(elem: A): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]
  def &(anotherSet:MySet[A]): MySet[A]

  def unary_! : MySet[A]
}


/*
EmptySet is a representation of the MySet trait but without any elements, it implements the methods taking into account that MySet will initially have no elements
*/
class EmptySet[A] extends MySet[A]:

  /*
  the contains method always returns false because an EmptySet is a representation of MySet but without elements
  */
  def contains(elem: A): Boolean = false


  /*
  the + method returns the creation of a NonEmptySet (implemented further down in the script) with the element passed as a parameter
  */
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)


  /*
  the ++ method only returns anotherset because the instantiated set that called the method does not have any elements
  */
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet


  /*
  all these methods that receive a function do not return anything or return a new instance of EmptySet because the EmptySet has no elements so there is no way to apply the functions so there is no return.
  */
  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = new EmptySet[A]
  def foreach(f: A => Unit): Unit = ()

  def -(elem: A): MySet[A] = new EmptySet[A]

  def --(anotherSet: MySet[A]): MySet[A] = new EmptySet[A]

  def &(anotherSet: MySet[A]): MySet[A] = new EmptySet[A]

  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)




class PropertyBasedSet[A](property: A => Boolean) extends MySet[A]:
  def contains(elem: A): Boolean = property(elem)
  def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)
  def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == anotherSet(x))

  def map[B](f: A => B): MySet[B] = politelyFail

  def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  def foreach(f: A => Unit): Unit = politelyFail
  def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))


  def -(elem: A): MySet[A] = filter(x => x != elem)
  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")


/*
NonEmptySet is the opposite of EmptySet, it has elements and the implementation of the methods has to take the elements that MySet has, its methods use a lot of recursion so that all elements are affected.
*/
class NonEmptySet[A](head:A, tail:MySet[A]) extends MySet[A] {

  /*
  the contains method has a very simple logic, if the element that the method received as a parameter is different from the head,
  it applies recursion and calls the contains of the tail that was passed in the construction of the NonEmptySet until it reaches the EmptySet and returns
  true in the first or second element comparison operation. As the construction of every MySet,
  regardless of size, is done from an EmptySet, which receives a head value,
  the recursion "deconstructs" the NonEmptySet by trying to "zero" the construction tree or finding the element.
  */
  def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)


  /*
  This method checks whether the element exists in the myset instance with the contains method,
  if not, it creates a new object passing the current object as tail and the new element as head.
  */
  def +(elem: A): MySet[A] =
    if (this contains elem) this
    else new NonEmptySet[A](elem, this)


  /*
  this method breaks down the current instance by element and adds it to the set passed in the parameter with recursion.
  [1 2 3] ++ [4 5] is
  [2 3] ++ [4 5] + 1 =
  [3] ++ [4 5] + 1 + 2 =
  [] ++ [4 5] + 1 + 2 + 3
  [4 5] ++ 1 + 2 + 3 == [4 5 1 2 3]
  */
  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head


  /*
  the HOF is map is well known and the implementation is quite simple,
  map receives a function as a parameter that receives only 1 parameter and is of the same type as the set elements and returns a new set with the result of the functions that received the elements as a parameter.
  In this case, the tail is a myset that has the map method so it can be called recursively, while the head is an element of type A that will not have the method so it will be passed in a conventional way.
  */
  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)


  /*
  flatmap is also well known and very similar to map but instead of the function returning an element of type B it expects the function to return a MySet[B] because that is what a flatmap does,
  the unfolding of a collection as a result to return only 1 collection.
  It also does this recursively and the application is very similar to map but instead of concatenating with the + method,
  which only inserts 1 element, it concatenates another set at each iteration.
  */
  def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)


  /*
  the filter method is also very similar to map and flatmap but it returns a collection with elements that respect the criteria that are passed in the function that the method receives as a parameter,
  in this case our filter receives a function that receives a parameter of the same type that our MySet elements and that returns a Boolean value. Here it applies recursively like the previous methods on the head element of the MySet,
  if the element matches the criteria it is added to the MySet which will be returned by the method, if not it is ignored and the flow continues until the MySet is empty.
  */
  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }


  /*
  the foreach method is different because it does not generate a return despite expecting an element of the same type as the MySet elements,
  this is because traditionally the foreach is for functions that have a return of type void or unit, that is, without a return, such as print functions and others.
  In this case it just iterates the MySet with recursion taking the head element (like the other methods) and executes the function that was passed as a parameter.
  */
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  def &(anotherSet: MySet[A]): MySet[A] =
    filter(anotherSet) /*intersection = filtering*/


  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))

}

object MySet {

  /*the apply method is applied whenever the MySet constructor is called
  MySet(1,2,3) = MySet.apply(1,2,3)

  the values parameter with * indicates that there will be several values that type.
  */

  def apply[A](values: A*): MySet[A] = {

    /*
    this function has the notation @tailrec which indicates that it is a recursive function.

    the apply function calls the buildSet function which is defined within the apply function itself,
    it passes the values parameter which is transformed into a sequence in the function call itself,
    in the second parameter, the accumulator it passes an EmptySet which was defined above.
    The buildSet function receives 2 parameters,
    a Sequence of type A (they are homogeneous parameters) and the accumulator which is of type MySet to be able to do the concatenation.
    If the valSeq parameter is empty (the isEmpty method returns true if the subsequent has an element and false if it does not) it returns the accumulator (end of the function),
    if the valSeq still has an element it will call itself passing the tail of the valSeq (all elements except the first)
    in the first parameter and in the second it will call the accumulator + the first element of valSeq (the head method returns the first element of the sequence).
    */
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, new EmptySet[A])
  }
}
