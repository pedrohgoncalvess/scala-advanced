package concurrency

import java.util.concurrent.Executors

object Intro extends App{

  /*
  interface Runnable {
  public void run()
  }
  */
  val runnable = new Runnable {
    override def run(): Unit = println("Running in parallel!")
  }

  val aThread = new Thread(runnable)

  aThread.run() //gives the signal to JVM start a JVM thread
  // create a JVM thread => OS thread
  runnable.run() //doesnt do anything in parallel
  aThread.join() // blocks until aThread finishes running
  //different runs produce different results

  //executors
  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => println("Something in the thread pool"))

  pool.execute(() => {
    Thread.sleep(1000)
    println("Done after 1 second")
  })

  pool.execute(() => {
    Thread.sleep(1000)
    println("Almost done")
    Thread.sleep(1000)
    println("Done after 2 seconds")
  })

  pool.shutdown() //disables the thread making it impossible to receive new tasks
  //pool.execute(() => println("should not appear")) //throws a exception in the calling thread

  //pool.shutdownNow() //disables the thread by throwing an exception on all tasks that are running, and no longer receives tasks
  println(pool.isShutdown) //should be true
}
