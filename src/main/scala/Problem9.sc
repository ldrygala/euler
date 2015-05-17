//a + b + c = 1000
//a2 + b2 = c2
//a = (500000 - 1000 b) / 1000 - b

val ab = for {
  n <- 1 until 500
  if ((500000 - 1000 * n) % (1000 - n) == 0)
} yield (n)

val c = 1000 - ab.sum

(c +: ab).product