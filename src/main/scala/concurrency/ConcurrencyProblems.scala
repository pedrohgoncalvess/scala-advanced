package concurrency


object JVMConcurrencyProblems extends App:
	
	var x = 0

	var thread1 = new Thread(() => {
		x = 1
	})

	var thread2 = new Thread(() => {
		x = 4
	})

	thread1.start
	thread2.start

	println(x) //race condition 
	//happens when different threads modify same value and final result depends on the specific order of execution

	case class BankAccount(var amount: Int)

	def buy(account: BankAccount, thing: String, price: Int): Unit =
		val temp = account.amount
		Thread.sleep(1)
		/*
		involves 3 steps
			- read old value
			- compute new value
			- write new value
		*/
		account.amount = temp - price

		def buy(account: BankAccount, thing: String, price: Int): Unit = {
			account.synchronized { //synchronized method does not allow multiple threads at the same time
				account.amount -= price
			}
		}

	/*
	race condition is multiple threads reading and writing the same value
	*/
	def demoBankingProblem: Unit = {
		(1 to 150).foreach { _ =>
			val account = new BankAccount(50000)
			val thread1 = new Thread(() => buy(account, "breads", 3000))
			val thread2 = new Thread(() => buy(account, "iphones", 4000))

			thread1.start
			thread2.start
			thread1.join
			thread2.join

			if (account.amount != 43000) println(s"I've just broken the bank: ${account.amount}")
		}
	}

	/*
	almost always, message = "Scala is awesome"
	is it guaranteed? NO
	Obnoxious situation (possible):

	main thread:
		message = "Scala sucks"
		awesomeThread.start()
		sleep(1001) - yields execution

	awesome thread:
		sleep(1000) - yields execution

	OS gives the CPU to some important thread, takes > 2s
	OS gives the CPU back to the main thread

	main thread:
		println(message) //"Scala sucks"
	awesome thread:
		message = "Scala is awesome"
	*/

	def demoSleepFallacy: Unit = {
		var message = ""
		val awesomeThread = new Thread(() => {
			Thread.sleep(1000)
			message = "Scala is awesome"
		})

		message = "Scala sucks"
		awesomeThread.start()
		Thread.sleep(1001)
		println(message)
	}

	demoSleepFallacy
