package s99

import Solutions.???

trait ArithmeticSolutions {

  // add new functions to integers
  implicit def extendInt(n: Int): ExtendedInt = ExtendedInt(n)

  case class ExtendedInt(n: Int) {

    def isPrime: Boolean = 
      n == 1 || n == 2 || (2 to n/2).filter(_%n == 0).size == 0

    def isCoprimeTo(other: Int): Boolean = gcd(n, other) == 1

    def totient: Int = (1 to n).filter(_.isCoprimeTo(n)).toList.size

    def improvedTotient: Int = ???

    def primeFactors: List[Int] = ???

    def primeFactorMultiplicity: List[(Int, Int)] = ???

    def primeFactorMultiplicityMap: Map[Int, Int] = ???

    def listPrimesinRange(r: Range): List[Int] = ???

    def goldbachs: List[(Int, Int)] = 
		for {
		  a <- listPrimesinRange(1 to n)
		  b <- 0 :: listPrimesinRange(1 to n)
		  if a + b == n
		} yield (a, b)

	def goldbach: (Int, Int) = goldbachs.headOption.getOrElse((n, n))
  }

  def primes: Stream[Int] = Stream.from(1).filter(_.isPrime)

  def gcd(m: Int, n: Int): Int = 
	if (m < n) gcd(n, m)
	else if (m % n == 0) n
	else gcd(m - m / n * n, n)

  def listPrimesinRange(r: Range): List[Int] = r.filter(_.isPrime).toList

  def printGoldbachList(r: Range): List[String] = ???

  def printGoldbachListLimited(r: Range, limit: Int): List[String] = ???

}
