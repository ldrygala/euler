val stream: Stream[Int] = Stream.cons(20, stream.map(_ + 1))
val isDivisible = (number: Int) => (1 to 20).forall(n => number % n == 0)

def time(f: => Option[Int]) = {
  val start: Long = System.currentTimeMillis()
  val result = f
  println((System.currentTimeMillis() - start) / 1000)
  result.get
}
time((20 to Integer.MAX_VALUE).find(isDivisible))
