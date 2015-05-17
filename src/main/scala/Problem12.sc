val triangles: Stream[Int] = Stream.from(2).map(n => (1 to n).sum)


val countDivs: (Int, Seq[Int]) => Int = (number: Int, divs: Seq[Int]) => {
  val div: Option[Int] = (2 to number).find(number % _ == 0)
  if (div.isDefined) {
    countDivs(number / div.get, div.get +: divs)
  } else {
    divs.groupBy(n => divs.count(n == _)).keys.map(_ + 1).product
  }
}
val divsSize = (n: Int) => {
  countDivs(n, Nil)
}

triangles.find(divsSize(_) > 500)

