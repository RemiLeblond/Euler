package scala.Euler

import scala.collection.mutable

object Euler032 {
  val digits = Set("1", "2", "3", "4", "5", "6", "7", "8", "9")

  def main(args: Array[String]) {
    // generer le premier nombre
    val start = Map("" -> digits)
    val level1 = start.flatMap(el => genPLusOne(el._1, el._2))
    val level2 = level1.flatMap(el => genPLusOne(el._1, el._2))
    val level3 = level2.flatMap(el => genPLusOne(el._1, el._2))
    val level4 = level3.flatMap(el => genPLusOne(el._1, el._2))

    val multip = level1 ++ level2 ++ level3 ++ level4

    val result = mutable.Set.empty[Int]

    for (m1 <- multip; m2 <- multip) {
      val m1p = m1._1.toInt
      val m2p = m2._1.toInt
      val prod = m1p * m2p
      if (ok(m1p, m2p, prod)) result += prod
    }

    var resultT = 0

    for (res <- result) resultT += res

    println(resultT)
    println(ok(123, 456, 788))

    println(level4("3456"))
    // generer le second en fonction
    // calculer le produit
    // checker que les bons nombres restent
  }

  def ok(n1: Int, n2: Int, n3: Int): Boolean = {
    val nbrs = mutable.Set.empty[String]
    for (char <- n1.toString) nbrs += char.toString
    for (char <- n2.toString) nbrs += char.toString
    for (char <- n3.toString) nbrs += char.toString

    nbrs.toSet == digits
  }

  def genPLusOne(nbr: String, rest: Set[String]): List[(String, Set[String])] = {
    (for (r <- rest) yield (r + nbr, rest - r)).toList
  }
}
