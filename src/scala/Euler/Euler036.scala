package scala.Euler

object Euler036 {
  def main(args: Array[String]) {
    var sum = 0
    for (i <- 1 to 1000000) {
      if (isPalindromic(i.toString)) {
        val iBase2 = base2(i)
        if (isPalindromic(iBase2)) {
          println(i + " " + iBase2)
          sum += i
        }
      }
    }

    println(sum)
  }

  def base2(i: Int): String = {
    val max = (Math.log(i)/Math.log(2)).toInt
    bbase2(i, max)
  }

  def bbase2(i: Int, max: Int): String = {
    if (max == 0) i.toString
    else {
      val bool = (i / Math.pow(2, max.toDouble)).toInt
      (bool.toString + bbase2(i - bool * Math.pow(2, max.toDouble).toInt, max - 1))
    }
  }

  def isPalindromic(s: String): Boolean = {
    s.reverse == s
  }
}
