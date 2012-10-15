package scala.Euler

object Euler034 {
  def main(args: Array[String]) {
    var result = 0
    for (i <- 3 to 5000000) {
      if (sumNbrFact(i) == i) {
        println(i)
        result += i
      }
    }
    println(result)
  }

  def sumNbrFact(i: Int): Int = {
    val se = for (n <- i.toString) yield {fact(n.toString.toInt)}
    val sum = se.foldLeft(0)((a, b) => a+b)
    sum
  }

  def fact(i: Int): Int = {
    if (i == 0) 1
    else i * fact(i - 1)
  }
}
