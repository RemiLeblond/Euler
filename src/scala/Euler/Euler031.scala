package scala.Euler

object Euler031 {
  def main(args: Array[String]) {
    var count = 0

    for (i <- 0 to 1) {
      for (j <- 0 to 2) {
        for (k <- 0 to 4) {
          for (l <- 0 to 10) {
            for (m <- 0 to 20) {
              for (n <- 0 to 40) {
                for (o <- 0 to 100) {
                  for (p <- 0 to 200) {
                    if (i*200+j*100+k*50+l*20+m*10+n*5+o*2+p*1 == 200) count += 1
                  }
                }
              }
            }
          }
        }
      }
    }

    println(count)
  }
}
