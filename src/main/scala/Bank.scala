
class Bank {
  val accounts = Seq.empty[String]

  def createAcc(acc: Account) = accounts :+ acc

}

class Account(balance: Int, number: Int)  extends Bank {
  var founds = balance

  def statement = founds

  def deposit(amount: Int) = founds = founds + amount

  def withdraw(amount: Int) = {
    if ((founds - amount) < 0 ) println("Not enough founds")
    else founds = founds - amount
  }
  def close = founds = 0

  def transfer(accountTo: Account, amount: Int) = {
    if (founds < amount) println("Not enough founds to make a transfer")
    else {
      founds = founds - amount
      accountTo.deposit(amount)
    }

  }
}
object Main1 extends App {
  val acc1 = new Account(0, 123)
  val acc2 = new Account(0, 124)
  val mybank = new Bank
  mybank.createAcc(acc1)
  println(acc1.statement)
  acc1.deposit(2000)
  println(acc1.statement)
  acc1.withdraw(1000)
  println(acc1.statement)
  println(acc2.statement)
  acc1.transfer(acc2, 500)
  println(acc1.statement)
  println(acc2.statement)

}

