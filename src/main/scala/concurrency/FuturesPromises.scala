package concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success}
import scala.concurrent.duration.*

//important for futures
import scala.concurrent.ExecutionContext.Implicits.global


object FuturesPromises extends App:
  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife // calculates the meaning of life on ANOTHER thread
  } // (global) which is passed by the compiler

  println(aFuture.value) //Option[Try[Int]]

  println("Waiting on the future")
  aFuture.onComplete { //works with pattern matching
    case Success(meaningOfLife) => println(s"The meaning of life is $meaningOfLife")
    case Failure(exception) => println(s"I have failed with $exception")
  } //SOME thread

  Thread.sleep(3000)

  // mini social network

  case class Profile(id: String, name: String):
    def poke(anotherProfile: Profile): Unit =
      println(s"${this.name} poking ${anotherProfile.name}")

  object SocialNetwork:
    // "database"
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy",
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    val random = new Random()

    def fetchProfile(id: String): Future[Profile] =
      //fetching from the DB / API / Any other
      Future {
        //Thread.sleep(random.nextInt(300))
        Profile(id, names(id))
      }

    def fetchBestFriend(profile: Profile): Future[Profile] =
      Future {
        //Thread.sleep(random.nextInt(400))
        val bfId = friends(profile.id)
        Profile(bfId, names(bfId))
      }

  //client: mark to poke bill
  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")

  //  mark.onComplete {
  //    case Success(markProfile) =>
  //      val bill = SocialNetwork.fetchBestFriend(markProfile)
  //      bill.onComplete {
  //        case Success(billProfile) => markProfile.poke(billProfile)
  //        case Failure(exception) => exception.printStackTrace()
  //      }
  //    case Failure(exception) => exception.printStackTrace()
  //  }

  //Thread.sleep(3000)
  /*
  onComplete is not asynchronous,
  it just registers a callback when the future is evaluated,
  which is why the thread's sleep is necessary
  */

  // functional composition of futures
  // map, flatMap, filter
  val nameOnTheWall = mark.map(profile => profile.name)
  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val zucksBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("Z"))

  //for-comprehensions
  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } yield mark.poke(bill)

  Thread.sleep(2000)

  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
  }

  val fallbackResult = SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))
  //if first future fails the second future is evaluated, if second future fails too, the first future exception will be thrown.

  // online banking app
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Pedro Banking"

    def fetchUser(name: String): Future[User] = Future {
      //simulate fetching from the database
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] =
      Future {
        //simulate some process
        Thread.sleep(500)
        Transaction(user.name, merchantName, amount, "SUCCESS")
      }

    def purchase(username: String, item: String, merchantName: String, cost: Double): Future[String] =
      //fetch the user from the database
      //create a transaction
      //WAIT for the transaction to finish

      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status
      transactionStatusFuture

      //Await.result(transactionStatusFuture, Duration.Inf)
  }

  //BankingApp.purchase("Pedro", "Iphone12", "pocilga store", 3000).foreach(x => println(x))

  val promise = Promise[Int]()
  val future = promise.future

  future.onComplete{
    case Success(r) => println("[consumer] I've received " + r)
  }

  val producer = new Thread(() => {
    println("[producer] crunching numbers...")
    Thread.sleep(1000)
    promise.success(42)
    println("[producer] done")
  })

  producer.start()
  Thread.sleep(1000)
