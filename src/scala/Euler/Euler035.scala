package scala.Euler

import scala.collection.mutable

object Euler035 {
  def main(args: Array[String]) {
    var count = 0
    for (i <- 2 to 1000000) {
      if (isPrime(i)) {
        val circSeti = circSet(i)
        val result = circSeti.foldLeft(true)((a, b) => (a && isPrime(b)))

        if (result) {
          println(i)
          count += 1
        }
      }
    }

    println("******** " + count)
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

  def circSet(i: Int): Set[Int] = {
    val circSet = mutable.Set.empty[Int]
    val iString = i.toString
    val iSize = iString.size
    for (ch <- 1 until iSize) {
      val (first, second) = (iString.substring(0, ch),
        iString.substring(ch, iSize))
      val circInt = (second + first).toInt
      circSet += circInt
    }
    circSet.toSet
  }
}
