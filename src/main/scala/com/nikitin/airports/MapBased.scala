package com.nikitin.airports

object MapBased {

  def main(): Unit = {

    val targetCountry = "Luxembourg"
    val targetCountryCode = getCountriesMap.getOrElse(targetCountry, "")
    val targetAirports = getAirports.getOrElse(targetCountryCode, List.empty)
    val runways = getRunways
    val targetRunways = targetAirports.map{ case (name, id) => name -> runways.get(id)}

    // println(targetRunways)


    println(s"Airports for $targetCountry with code $targetCountryCode:")

    targetRunways.foreach{
      case (name, None) => println(s"$name.")
      case (name, Some(list)) => println(s"$name"); list.foreach(println)
    }


  }


  def getRunways = {
    val runwaysBuffer = io.Source.fromResource("runways.csv")

    val runways = runwaysBuffer.getLines().drop(1).map{ line =>
      val runway = line.split(",").map(_.trim.replaceAll("\"", ""))
      val airport = runway(2)
      val surface = runway(5)

      (airport -> surface)
    }.toList
      .groupBy(_._1)
      .mapValues( _.map(_._2))


    runwaysBuffer.close()

    runways
  }

  def getCountriesMap = {

    val countriesBuffer = io.Source.fromResource("countries.csv")

    val countries = countriesBuffer.getLines().drop(1).map { line =>
      val a = line.split(",").map(_.trim.replaceAll("\"", ""))
      val code = a(1)
      val name = a(2)
      name -> code
    }.foldLeft(Map.empty[String, String])((map, el) => map updated(el._1, el._2))

    countriesBuffer.close()

    countries
  }

  def getAirports = {
    val airportsBuffer = io.Source.fromResource("airports.csv")

    val airports = airportsBuffer.getLines().drop(1).map { line =>
      val airport = line.split(",").map(_.trim.replaceAll("\"", ""))
      val ident = airport(1)
      val name = airport(3)
      val country = airport(8)

      (country -> (name -> ident))
    }.toList
      .groupBy(_._1)
      .mapValues(_.map{ case (_, (n, i)) => (n, i)})

    airportsBuffer.close()

    airports
  }

}
