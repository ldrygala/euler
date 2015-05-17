def isPalindrome(number: Int) = number == number.toString.reverse.toInt

(for {
  i <- 100 to 999
  j <- 100 to 999
  palindrome = i * j if isPalindrome(i * j)
} yield (palindrome)).max