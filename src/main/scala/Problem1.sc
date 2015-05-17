def multipleBy(by: Int*) = (number: Int) => by.exists(number % _ == 0)

def sum(limit: Int) = 1 until limit filter multipleBy(3, 5) sum

sum(1000)