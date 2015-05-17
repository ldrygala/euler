val primeNumberIndex = 10001
val numbers: Stream[Int] = Stream.from(3)
val isPrime = (n: Int) => {
  2 until n forall (n % _ != 0)
}
numbers.filter(isPrime)(primeNumberIndex - 2)
