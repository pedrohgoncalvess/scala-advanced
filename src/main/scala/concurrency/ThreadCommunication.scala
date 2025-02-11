package concurrency

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App:
  /*
    the produces-consumer problem

    producer -> [ x ] -> consumer
  */

  class SimpleContainer:
    private var value: Int = 0

    def isEmpty: Boolean = value == 0
    def set(nv: Int) = value = nv
    def get: Int =
      val result = value
      value = 0
      result

  def naiveProdCons(): Unit =
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while(container.isEmpty) {
        println("[consumer] still waiting...")
      }
      println(s"[consumer] I have consumed: ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(150)
      val value = 42
      container.set(value)
      println(s"[producer] I have produced, after long work the value: $value")
    })

    consumer.start()
    producer.start()


  //wait and notify
  def smartProdCons(): Unit =
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      container.synchronized{
        container.wait()
      }
      println(s"[consumer] I have consumed ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] Hard at work...")
      Thread.sleep(2000)
      val value = 42

      container.synchronized{
        println(s"[producer] I'm producing $value")
        container.set(value)
        container.notify()
      }
    })

    consumer.start()
    producer.start()
    /*
    producer -> [? ? ?] -> consumer
    */

  def prodConsLargeBuffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    val consumer = new Thread(() => {
      val random = new Random()

      while (true) {
        buffer.synchronized{
          if (buffer.isEmpty) {
            println("[consumer] buffer empty, waiting...")
            buffer.wait()
          }

          //there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer] consumed: $x")

          buffer.notify() //send signal to producer like "hey producer, there's empty space available, are you lazy?
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0

      while(true) {
        buffer.synchronized {
          if(buffer.size == capacity) {
            println("[producer] buffer is full, waiting...")
            buffer.wait()
          }

          //there must be at least ONE EMPTY SPACE in the buffer
          println(s"[producer] producing: $i")
          buffer.enqueue(i)

          buffer.notify() //send a signal to producer

          i += 1
        }

        Thread.sleep(random.nextInt(250))
      }
    })

    consumer.start()
    producer.start()
  }

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread:
    override def run: Unit =
      val random = new Random()

      while (true) {
        buffer.synchronized {
          while (buffer.isEmpty) {
            println(s"[consumer $id] buffer empty, waiting...")
            buffer.wait()
          }

          //there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer] consumed: $x")

          buffer.notify() //send signal to producer like "hey producer, there's empty space available, are you lazy?
        }
        Thread.sleep(random.nextInt(500))
      }

  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread:
    override def run: Unit =
      val random = new Random()
      var i = 0

      while (true) {
        buffer.synchronized {
          while (buffer.size == capacity) {
            println(s"[producer $id] buffer is full, waiting...")
            buffer.wait()
          }

          //there must be at least ONE EMPTY SPACE in the buffer
          println(s"[producer $id] producing: $i")
          buffer.enqueue(i)

          buffer.notify() //send a signal to producer

          i += 1
        }

        Thread.sleep(random.nextInt(250))
      }

  def multipleProducersConsumers(nConsumers: Int, nProducers:Int): Unit =
    val buffer = mutable.Queue[Int]()
    (1 to nConsumers).foreach(n => Consumer(n, buffer).start())
    (1 to nProducers).foreach(n => Producer(n, buffer, 3).start())


  //multipleProducersConsumers(4, 4)

  /*
    Exercises.

    1) think of an example where notifyAll acts in a different way than notify?
    2) create a deadlock
    3) create a livelock
  */

  def testNotifyAll(): Unit =
    val bell = new Object

    (1 to 10).foreach(i => new Thread(() => {
      bell.synchronized{
        println(s"[thread $i] waiting...")
        bell.wait()
        println(s"[thread $i] hooray!")
      }
    }).start())

    new Thread(() => {
      Thread.sleep(2000)
      println("[announcer] Rock'n roll!")
      bell.synchronized{
        bell.notify()
      }
    }).start()

//  testNotifyAll()

  case class Friend(name: String):
    def bow(other: Friend): Unit =
      this.synchronized {
        println(s"$this: I am bowing to my friend $other.")
        other.rise(this)
        println(s"$this: my friend $other has risen.")
      }

    def rise(other: Friend): Unit =
      this.synchronized {
        println(s"$this: I am rising to my friend $other")
      }

    var side = "right"
    def switchSide(): Unit = if side == "right" then side = "left" else side = "right"

    def pass(other: Friend): Unit =
      while (this.side == other.side) {
        println(s"$this: Oh, but please, $other, feel free to pass...")
        switchSide()
        Thread.sleep(1000)
      }

  val sam = Friend("Sam")
  val pierre = Friend("Pierre")

//  new Thread(() => sam.bow(pierre)).start() // sam's lock       |   then pierre's lock
//  new Thread(() => sam.bow(sam)).start()    // pierres's lock   |   then sam's lock

  new Thread(() => sam.pass(pierre)).start()
  new Thread(() => pierre.pass(sam)).start()
