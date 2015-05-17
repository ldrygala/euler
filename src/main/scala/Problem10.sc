val primeAbove: (Int, Stream[Int]) => Int = (n, primes) => {
  if (primes.takeWhile(_ <= Math.sqrt(n)).forall(n % _ != 0)) {
    n
  } else {
    primeAbove(n + 1, primes)
  }
}

val numbers: Stream[Int] = 2 #:: numbers.map(n => primeAbove(n + 1, numbers))
numbers.takeWhile(_ < 2000000).foldLeft(BigInt(0))(_ + _)
//numbers.takeWhile(_ < 2000000).sum