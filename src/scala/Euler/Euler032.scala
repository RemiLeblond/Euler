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

    for (m <- multip) {
      val toTest = m._1.size match {
        case 4 => Map("" -> m._2).flatMap(el => genPLusOne(el._1, el._2))
        case 3 =>
          val level31 = Map("" -> m._2).flatMap(el => genPLusOne(el._1, el._2))
          val level32 = level31.flatMap(el => genPLusOne(el._1, el._2))
          level31 ++ level32
        case 2 =>
          val level21 = Map("" -> m._2).flatMap(el => genPLusOne(el._1, el._2))
          val level22 = level21.flatMap(el => genPLusOne(el._1, el._2))
          val level23 = level22.flatMap(el => genPLusOne(el._1, el._2))
          level21 ++ level22 ++ level23
        case 1 =>
          val level11 = Map("" -> m._2).flatMap(el => genPLusOne(el._1, el._2))
          val level12 = level11.flatMap(el => genPLusOne(el._1, el._2))
          val level13 = level12.flatMap(el => genPLusOne(el._1, el._2))
          val level14 = level13.flatMap(el => genPLusOne(el._1, el._2))
          level11 ++ level12 ++ level13 ++ level14
      }

      for (tt <- toTest) {
        val m1 = m._1.toInt
        val tt1 = tt._1.toInt
        val prod = m1 * tt1
        if (ok(m1, tt1, prod)) result += prod
      }
    }

    var resultT = 0

    for (res <- result) resultT += res

    println(resultT)
  }

  def ok(n1: Int, n2: Int, n3: Int): Boolean = {
    var nbrs = List.empty[String]
    for (char <- n1.toString) nbrs = char.toString::nbrs
    for (char <- n2.toString) nbrs = char.toString::nbrs
    for (char <- n3.toString) nbrs = char.toString::nbrs

    (nbrs.toSet == digits) && (nbrs.size == 9)
  }

  def genPLusOne(nbr: String, rest: Set[String]): List[(String, Set[String])] = {
    (for (r <- rest) yield (r + nbr, rest - r)).toList
  }
}
