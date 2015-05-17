lazy val fib: Stream[Int] = 1 #:: 2 #:: fib.zip(fib.tail).map(n => n._1 + n._2)

fib.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum