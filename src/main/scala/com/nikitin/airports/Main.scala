package com.nikitin.airports

import com.nikitin.airports.MapBased.{getAirports, getCountriesMap, getRunways}
import com.nikitin.airports.ObjectBased.{Airport, Country, Runway}

import scala.collection.immutable.HashMap


object Main extends App {


  override def main(args: Array[String]): Unit = {


      print("Object based  ")
      measure(objectBased)
      print("Map based  ")
   //   measure(mapBased)
   // MapBased.main()


  }

  def objectBased = {
    val countries: HashMap[String, Country] = ObjectBased.createCountryMap
    val airports: HashMap[String, Airport] = ObjectBased.createAirportMap
    val runways: HashMap[String, Runway] = ObjectBased.createRunwayMap()

    val country = "Ukraine"
    val countryCode = countries.getOrElse(country,Country(0,"","","","",None)).code
    val countryAirports = airports.filter{case (_, airport) => airport.country == countryCode}
    val runwaysForAirport = countryAirports.mapValues(airport =>
      runways.collect{case (_, runway) if runway.airport == airport.ident => runway})

    countryAirports.foreach { case (ident , airport) =>
    println(airport)
      val runways = runwaysForAirport(ident)
      if(runways.nonEmpty)
         runways.foreach(r => println("     " + r))
    }

 //   println(runwaysForAirport)

  }
  def mapBased = {
    val countries = getCountriesMap
    val airports = getAirports
    val runways = getRunways
  }

  def measure(f: => Unit) = {
    val startTime = System.currentTimeMillis()

    f

    val timeElapsed = System.currentTimeMillis() - startTime
    val timeElapsedSec = timeElapsed / 1000

    println(s"Time: $timeElapsed ms")
  }
}