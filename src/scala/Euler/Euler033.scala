package scala.Euler

import scala.collection.mutable

object Euler033 {
  def main(args: Array[String]) {
    println(returnDen(getWeirdFractions(99, 99)))
  }

  def getWeirdFractions(numMax: Int, denMax: Int): Set[(Int, Int)] = {
    val result = mutable.Set.empty[(Int, Int)]
    for (num <- 10 to numMax) {
      for (den <- 10 to denMax) {
        if (num % 10 == 0 || den % 10 == 0 || num >= den) {}
        else {
          val (num1, num2) = (num/10, num%10)
          val (den1, den2) = (den/10, den%10)

          if ((num1 == den1 && num * den2 == num2 * den)
            || (num1 == den2 && num * den1 == num2 * den)
            || (num2 == den1 && num * den2 == num1 * den)
            || (num2 == den2 && num * den1 == num1 * den)) {
            result += ((num, den))
          }
        }
      }
    }

    result.toSet
  }

  def returnDen(input: Set[(Int, Int)]): Int = {
    val (pNum, pDen) = input.foldLeft(1, 1)((start, input) => (start._1 * input._1, start._2 * input._2))
    pDen / gcd(pDen, pNum)
  }

  def gcd(a: Int, b: Int): Int = {
    if (a < b) gcd(b, a)
    else if (a % b == 0) b
    else (gcd(a, a % b))
  }
}
