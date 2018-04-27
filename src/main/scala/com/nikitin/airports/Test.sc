val countriesBuffer = io.Source.fromResource("countries.csv")
//val countries: Map[String, String]

val countries = countriesBuffer.getLines().drop(1).take(5).map { line =>
  val a = line.split(",").map(_.trim.replaceAll("\"", ""))
  val code = a(1)
  val name = a(2)
  name -> code
}


println(countries)

countriesBuffer.close()