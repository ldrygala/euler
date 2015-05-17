val sumOfSquare: Long = (1 to 100).map(x => x * x).sum
val squareOfSum: Long = Math.pow((1 to 100).sum, 2).toLong
squareOfSum - sumOfSquare