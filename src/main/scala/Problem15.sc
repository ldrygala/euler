val limit = 20
val points = Array.ofDim[BigInt](limit + 1, limit + 1)

0 to limit foreach { i =>
  points(0)(i) = 1
  points(i)(0) = 1
}
for {
  i <- 1 to limit
  j <- 1 to limit
} points(i)(j) = points(i - 1)(j) + points(i)(j - 1)

points(limit)(limit)