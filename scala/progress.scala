
def progress(i: Int, indicator: String = "-") = {
  val percent = s"$i%"
  val padding = (0 until (4 - p.size)).map(_ => " ").mkString
  val dashes = indicator * (i - p.size)
  s"$padding$percent $dashes"
}

for (i <- 0 to 100) {
  print(progress(i))
  Thread.sleep(100)
  print("\r")
}
println

