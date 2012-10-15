package scala.test

import collection.mutable

object test {
  def main(args: Array[String]) {
    println(buildList(3797))
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
}
