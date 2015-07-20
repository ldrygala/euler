def sumOfDevisors(n:Int) = (1 until n).filter(n%_==0).sum

def isAmicable(n:Int) = {
  val divisorsSum: Int = sumOfDevisors(n)
  if(divisorsSum != n) {
    n == sumOfDevisors(divisorsSum)
  } else {
    false
  }
}

(1 until 10000).filter(isAmicable(_)).sum