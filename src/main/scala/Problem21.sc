def sumOfDevisors(n:Int) = (1 until n).filter(n%_==0).sum

def isAmicable(n:Int) = {
  val devisorsSum: Int = sumOfDevisors(n)
  if(devisorsSum != n) {
    n == sumOfDevisors(devisorsSum)
  } else {
    false
  }
}

(1 until 10000).filter(isAmicable(_)).sum