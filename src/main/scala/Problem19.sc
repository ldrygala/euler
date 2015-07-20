import java.util.{Calendar, Date}

val monthYears: IndexedSeq[(Int, Int)] = for {
  y <- 1901 to 2000
  m <- 0 to 11
} yield (m, y)

monthYears.filter {
  case (m,y) => {
    val calendar: Calendar = Calendar.getInstance()
    calendar.set(y,m,1)
    calendar.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY
  }
}.size
