def sumNumbers(number: BigInt) = number.toString().toList.map(_.toString.toInt).reduce(_ + _)
val number = BigInt(2).pow(1000)
sumNumbers(number)
