package scala.Euler

import scala.collection.mutable

object Euler037 {
  def main(args: Array[String]) {
    println(getXFirst(11))
  }

  def getXFirst(limit: Int): Int = {
    var count = 0
    var sum = 0
    var test = 10
    while (count < limit) {
      if (isPrime(test)) {
        val truncSet = buildList(test)
        val bool = truncSet.foldLeft(true)((a, b) => (a && isPrime(b)))
        if (bool) {
          println(test + " " + truncSet)
          count += 1
          sum += test
        }
      }
      test += 1
    }

    sum
  }

  def buildList(i: Int): Set[Int] = {
    val result = mutable.Set.empty[Int]
    val iString = i.toString
    val iSize = iString.size
    for (n <- 1 until iSize) {
      result += iString.substring(n, iSize).toInt
      result += iString.substring(0, iSize - n).toInt
    }

    result.toSet
  }

  def isPrime(i: Int): Boolean = {
    if (i == 1) false
    else if (i < 4) true
    else if (i % 2 == 0) false
    else if (i < 9) true
    else if (i % 3 == 0) false
    else {
      val r = Math.floor(Math.sqrt(i))
      var f = 5
      while (f <= r) {
        if (i % f == 0) return false
        if (i % (f + 2) == 0) return false
        f += 6
      }
      true
    }
  }
}
