(1 to 100).foldLeft(BigInt(1))(_*_).toString.toList.map(_.toString.toInt).reduce(_+_)
