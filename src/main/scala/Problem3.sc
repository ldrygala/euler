val number = 600851475143l

val maxPrime = Math.sqrt(number).toInt

val stream: Stream[Int] = Stream.cons(2, stream.map(_ + 1)).take(maxPrime)

stream.filter(n => number % n == 0 && stream.take(n - 2).forall(n % _ != 0)).last