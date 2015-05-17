val numbersMap = Map(
  0 -> 0,
  1 -> 3,
  2 -> 3,
  3 -> 5,
  4 -> 4,
  5 -> 4,
  6 -> 3,
  7 -> 5,
  8 -> 5,
  9 -> 4,
  10 -> 3,
  11 -> 6,
  12 -> 6,
  13 -> 8,
  14 -> 8,
  15 -> 7,
  16 -> 7,
  17 -> 9,
  18 -> 8,
  19 -> 8,
  20 -> 6,
  30 -> 6,
  40 -> 5,
  50 -> 5,
  60 -> 5,
  70 -> 7,
  80 -> 6,
  90 -> 6,
  100 -> 7,
  1000 -> 8
)

def number2String(number: Int): Int = {
  number match {
    case 1000 => "one".size + numbersMap(1000)
    case n if n < 20 => numbersMap(n)
    case n if n < 100 => numbersMap(n / 10 * 10) + numbersMap(n % 10)
    case n if n % 100 == 0 => numbersMap(n / 100) + numbersMap(100)
    case n if n < 1000 => numbersMap(n / 100) + numbersMap(100) + "and".size + number2String(n % 100)
  }
}
(1 to 1000).map(number2String(_)).sum
