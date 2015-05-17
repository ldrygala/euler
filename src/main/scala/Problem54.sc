import scala.io.Source
import scala.math.Ordering


case class Card(value: Value, color: Color) {
  override def toString = value.symbol.toString + color.symbol.toString

  def isValuesEqual(others: Card*) = others.forall(_.value.value == this.value.value)
}

implicit val cardOrdering = new Ordering[Card] {

  override def compare(x: Card, y: Card): Int = x.value.value.compare(y.value.value)
}

trait Symbol {
  val symbol: Char
}

sealed trait Color extends Symbol {
  override def toString = symbol.toString
}

object Hearts extends Color {
  override val symbol = 'H'
}

object Diamonds extends Color {
  override val symbol = 'D'
}

object Clubs extends Color {
  override val symbol = 'C'
}

object Spades extends Color {
  override val symbol = 'S'
}

sealed trait Value extends Symbol {
  val value: Int

  def of(color: Color) = Card(this, color)

  def -(other: Value) = this.value - other.value


  override def toString = symbol.toString
}

object Two extends Value {

  override val symbol = '2'
  override val value: Int = 2
}

object Three extends Value {
  override val symbol = '3'
  override val value: Int = 3
}

object Four extends Value {
  override val symbol = '4'
  override val value: Int = 4
}

object Five extends Value {
  override val symbol = '5'
  override val value: Int = 5
}

object Six extends Value {
  override val symbol = '6'
  override val value: Int = 6
}

object Seven extends Value {
  override val symbol = '7'
  override val value: Int = 7
}

object Eight extends Value {
  override val symbol = '8'
  override val value: Int = 8
}

object Nine extends Value {
  override val symbol = '9'
  override val value: Int = 9
}

object Ten extends Value {
  override val symbol = 'T'
  override val value: Int = 10
}

object Jack extends Value {
  override val symbol = 'J'
  override val value: Int = 11
}

object Queen extends Value {
  override val symbol = 'Q'
  override val value: Int = 12
}

object King extends Value {
  override val symbol = 'K'
  override val value: Int = 13
}

object Ace extends Value {
  override val symbol = 'A'
  override val value: Int = 14
}

val isConsecutiveValues: (Seq[Card] => Boolean) = (cards) => {
  cards match {
    case Seq(f, s) => if (s.value - f.value == 1) true else false
    case Seq(f, s, _*) => if (s.value - f.value == 1) true && isConsecutiveValues(cards.tail) else false
  }
}

val isRoyalFlush = (cards: Seq[Card]) => {
  val color: Color = cards.head.color
  val royalFlushCards = Seq(Ten.of(color), Jack.of(color), Queen.of(color), King.of(color), Ace.of(color))
  cards.forall(card => royalFlushCards.exists(_ == card))
}

val isStraightFlush = (cards: Seq[Card]) => {
  val color: Color = cards.head.color
  cards.forall(_.color == color) && isConsecutiveValues(cards.sorted)
}

val isFourOfKind = (cards: Seq[Card]) => {
  val sortedCards: Seq[Card] = cards.sorted
  sortedCards match {
    case Seq(first, second, third, fourth, five) => first.isValuesEqual(second, third, fourth) || first.isValuesEqual(second, third, fourth)
    case _ => false
  }
}

val isFullHouse = (cards: Seq[Card]) => {
  val sortedCards: Seq[Card] = cards.sorted
  sortedCards match {
    case Seq(first, second, third, fourth, five) => (first.isValuesEqual(second, third) && fourth.isValuesEqual(five)) || (first.isValuesEqual(second) && third.isValuesEqual(fourth, five))
    case _ => false
  }
}

val isFlush = (cards: Seq[Card]) => {
  cards.forall(_.color == cards.head.color)
}

val isStraight = (cards: Seq[Card]) => {
  isConsecutiveValues(cards.sorted)
}

val isThreeOfKind = (cards: Seq[Card]) => {
  cards.exists(card => cards.filter(_.isValuesEqual(card)).size == 3)
}

val isTwoPairs = (cards: Seq[Card]) => {
  cards.filter { card =>
    cards.filter(_.isValuesEqual(card)).size == 2
  }.size == 4
}

val fineOnePair = (cards: Seq[Card]) => {
  cards.filter { card =>
    cards.filter(_.isValuesEqual(card)).size == 2
  }
}
val isOnePairs = (cards: Seq[Card]) => {
  fineOnePair(cards).size == 2
}

val draw = (drawNumber: Int, hands: (Seq[Card], Seq[Card])) => {
  drawNumber match {
    case 8 => if (fineOnePair(hands._1).head.value.value > fineOnePair(hands._2).head.value.value) Some(1) else Some(2)
  }
}


val values = Seq(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
val colors = Seq(Hearts, Diamonds, Clubs, Spades)
val hands = Seq(isRoyalFlush, isStraightFlush, isFourOfKind, isFullHouse, isFlush, isStraight, isThreeOfKind, isTwoPairs, isOnePairs).zipWithIndex

val firstLine = "5H 5C 6S 7S KD 2C 3S 8S 8D TD"

val parseCards = (line: String) => {
  line.split(" ").map {
    case card => values.find(_.symbol == card.head).get.of(colors.find(_.symbol == card.last).get)
  }.toSeq
}

case class Result(hand1: Option[Int], hand2: Option[Int])

val whoWin = (line: Seq[Card]) => {
  val (hand1, hand2) = line.splitAt(5)
  val hand1Result: Option[Int] = hands.find(hand => hand._1(hand1)).map(_._2)
  val hand2Result: Option[Int] = hands.find(hand => hand._1(hand2)).map(_._2)
  Result(hand1Result, hand2Result) match {
    case Result(None, None) => if (hand1.max.value.value > hand2.max.value.value) Some(1) else Some(2)
    case Result(None, Some(_)) => Some(2)
    case Result(Some(_), None) => Some(1)
    case Result(Some(h1), Some(h2)) if h1 == h2 => draw(h1, (hand1, hand2))
    case Result(Some(h1), Some(h2)) if h1 != h2 => if (h1 < h2) Some(1) else Some(2)
    case _ => None
  }
}
Source.fromURL(getClass.getResource("poker.txt")).getLines().map(parseCards).map(whoWin).filter(_.get == 1).size
