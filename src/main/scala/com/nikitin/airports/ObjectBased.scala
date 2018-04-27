package com.nikitin.airports

import scala.collection.immutable.HashMap

object ObjectBased {
  //"id","code","name","continent","wikipedia_link","keywords"
  case class Country(id: Long,
                     code: String,
                     name: String,
                     continent: String,
                     wikipedia: String,
                     keywords: Option[List[String]])

  object Country {
     def createFromCSVLine(line: String): Country = {
       splitAndTrim(line) match {
        case id :: code :: name :: continent :: wikipedia :: Nil => Country(id.toLong, code, name, continent, wikipedia, None)
        case id :: code :: name :: continent :: wikipedia :: keywords => Country(id.toLong, code, name, continent, wikipedia, Some(keywords))
      }
    }
  }

  //"id","ident","type","name","latitude_deg","longitude_deg","elevation_ft","continent","iso_country","iso_region","municipality","scheduled_service","gps_code","iata_code","local_code","home_link","wikipedia_link","keywords"
  case class Airport (id: Long,
                      ident: String,
                      `type`: String,
                      name: String,
                      country: String)

  object Airport {
    def createFromCSVLine(line: String): Airport = {
      splitAndTrim(line) match {
        case id :: ident :: aType :: name :: _ :: _ :: _ :: _ :: country :: _ =>
          Airport(id.toLong, ident, aType, name, country)
      }
    }
  }
  //"id","airport_ref","airport_ident","length_ft","width_ft","surface","lighted","closed","le_ident","le_latitude_deg","le_longitude_deg","le_elevation_ft","le_heading_degT","le_displaced_threshold_ft","he_ident","he_latitude_deg","he_longitude_deg","he_elevation_ft","he_heading_degT","he_displaced_threshold_ft",
  case class Runway(id: String,
                    airport: String,
                    length: String,
                    width: String,
                    surface: String,
                    lighted: String,
                    closed: String)


  object Runway {

    def createFromCSVLine(line: String): Runway = {
      splitAndTrim(line) match {
        case id :: _ :: airport :: length :: width :: surface :: lighted :: closed  :: _ =>
          Runway(id, airport, length, width, surface, lighted, closed)
      }
    }
  }
  //TODO: Generalize functions. pass function for create and the resource name
  def createCountryMap(): HashMap[String, Country] = {
    val countriesBuffer = io.Source.fromResource("countries.csv")

    val countries = countriesBuffer
      .getLines()
      .drop(1)
      .map(Country.createFromCSVLine)
      .foldLeft(HashMap.empty[String, Country])((map, country) => map updated(country.name, country))

    countriesBuffer.close()
    countries
  }
  def createAirportMap(): HashMap[String, Airport] = {
    val airportBuffer = io.Source.fromResource("airports.csv")

    val airports = airportBuffer
      .getLines()
      .drop(1)
      .map(Airport.createFromCSVLine)
      .foldLeft(HashMap.empty[String,Airport])((map, airport) => map updated(airport.ident, airport))

    airportBuffer.close()
    airports
  }
  def createRunwayMap(): HashMap[String, Runway] = {
    val runwaysBuffer = io.Source.fromResource("runways.csv")

    val runways = runwaysBuffer
      .getLines()
      .drop(1)
      .map(Runway.createFromCSVLine)
      .foldLeft(HashMap.empty[String,Runway])((map, runway) => map updated(runway.id, runway))

    runwaysBuffer.close()
    runways
  }

  protected def splitAndTrim (line: String): List[String] =
    line.split(",").map(_.trim.replace("\"","")).toList

}
