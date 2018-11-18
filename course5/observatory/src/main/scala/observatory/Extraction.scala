package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

/**
  * 1st milestone: data extraction
  */
object Extraction {

  lazy val spark: SparkSession = {
    SparkSession.builder().appName("Extraction").config("spark.master", "local").getOrCreate()
  }

  import spark.implicits._

  def fahrenheitToCelsius(fahrenheit: Double) = BigDecimal((fahrenheit - 32) / 1.8).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

  def getPath(file: String): String = {
    Paths.get(getClass.getResource(file).toURI).toString
  }
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationData = readStations(stationsFile)
    val temperatureData = readTemperatures(temperaturesFile)
    stationData.join(temperatureData).where(stationData.col("STN") === temperatureData.col("STN") && stationData.col("WBAN") === temperatureData.col("WBAN"))
      .selectExpr("Month", "Day", "Latitude", "Longitude", "Temperature")
      //      .selectExpr("Month", "Day", "Latitude", "Longitude", "(Temperature - 32) * 5 / 9")
      .collect().toList.map(row => (LocalDate.of(year.asInstanceOf[Int], row(0).asInstanceOf[Int], row(1).asInstanceOf[Int]), new Location(row(2).asInstanceOf[Double], row(3). asInstanceOf[Double]), row(4).asInstanceOf[String].toDouble))
  }

  def readStations(stationsFile: String): DataFrame = {
    val schema = StructType(Array(
      StructField("STN", StringType, true),
      StructField("WBAN", StringType, true),
      StructField("Latitude", DoubleType, true),
      StructField("Longitude", DoubleType, true)
    ))
    spark.read.schema(schema).csv(getPath(stationsFile)).filter($"Latitude".isNotNull && $"Longitude".isNotNull).na.fill("-")
  }

  def readTemperatures(temperaturesFile: String): DataFrame = {
    val schema = StructType(Array(
      StructField("STN", StringType, true),
      StructField("WBAN", StringType, true),
      StructField("Month", IntegerType, false),
      StructField("Day", IntegerType, false),
      StructField("Temperature", DoubleType, false)
    ))
    spark.read.schema(schema).csv(getPath(temperaturesFile))
      .filter($"Month".isNotNull && $"Day".isNotNull && $"Temperature".isNotNull)
      .withColumn("Temperature", format_number(($"Temperature" - 32.0) * 5.0 / 9.0, 1))
      .na.fill("-")
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.par.groupBy(_._2).mapValues(
      v => v.foldLeft(0.0)((avgTemp, item) => avgTemp + (item._3/v.size))
    ).seq
  }
}
