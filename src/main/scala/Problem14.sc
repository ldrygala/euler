val startsNumbers = 1 until 1000000
val isEven = (n: BigInt) => n % 2 == 0

val collatz: (BigInt, Int) => Int = (number, size) => {
  if (number == 1) {
    size + 1
  } else if (isEven(number)) {
    collatz(number / 2, size + 1)
  } else {
    collatz(number * 3 + 1, size + 1)
  }

}
startsNumbers.map(n => (n, collatz(n, 0))).maxBy(_._2)
