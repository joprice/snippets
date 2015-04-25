
def renderTable(rows: Seq[Seq[String]]) = {
  val colCount = rows(0).size
  val maxes = new Array[Int](colCount)
  val rowCount = rows.size

  (0 until rowCount).foreach { i =>
    (0 until colCount).foreach { j =>
      val k = rows(i)(j).size
      if (k > maxes(j)) {
        maxes(j) = k
      }
    }
  }

  rows.foreach { row =>
    row.zipWithIndex.foreach { case (col, i) =>
      val max = maxes(i)
      val size = col.size
      val padded = if (size < max) col + (" " * (max - size)) else col
      print(padded)
      print("\t")
    }
    println
  }
}
