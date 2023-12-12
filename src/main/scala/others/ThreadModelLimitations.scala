package others

import scala.concurrent.Future

object ThreadModelLimitations extends App{

  /*POINT 1:
      /*encapsulation is broken in a multithread env*/
      /*synchronization locks the rescue*/
  */


  class BankAccount(private var mount: Int):

    override def toString: String = "" + mount

    def withDraw(money: Int) = this.mount.synchronized {
      this.mount -= money
    }

    def deposit(money:Int) = this.mount.synchronized{
      this.mount += money
    }

    def getMount: Int =
      this.mount


//  val account = new BankAccount(2000)
//
//  for (_ <- 1 to 100) new Thread(() => account.deposit(1)).start()
//
//  for (_ <- 1 to 1000) new Thread(() => account.withDraw(1)).start()
//
//  println(account.getMount)

/*POINT 2: DELEGATING SOMETHING TO A THREAD IS VERY COMPLEX*/

  var task: Runnable = null

  val runningThread: Thread = new Thread(() => {
    while(true) {
      while(task == null) {
        runningThread.synchronized({
          println("[background] waiting for a task...")
          runningThread.wait()
        })
      }
    }
    task.synchronized {
      println("[background] I have a task....")
      task.run()
      task = null
    }
  })

  def delegateToBackground(r: Runnable) = {
    if (task == null) task = r
    runningThread.synchronized{
      runningThread.notify()
    }
  }

  runningThread.start()
  Thread.sleep(1000)
  delegateToBackground(() => println(42))
  Thread.sleep(1000)
  delegateToBackground(() => println("This should run in the background."))

  /*POINT 3 TRACING AND DEALING WITH ERRORS IN A MULTITHREAD IS A PAINT IN THE NECK*/

  import scala.concurrent.ExecutionContext.Implicits.global

  val futures = (0 to 9).
    map(i => 100000 * i until 100000 * (i + 1)).
    map(range => Future{
      if (range.contains(546735)) throw new RuntimeException("invalid number")
      range.sum
    })

  val sumFuture = Future.reduceLeft(futures)(_ + _)
}
