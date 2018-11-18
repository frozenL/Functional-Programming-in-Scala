package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location, p: Int = 2): Temperature = {
    val distances = temperatures.map{ case(loc, temp) => (loc distance location, temp) }
    distances.find(_._1 == 0).map(_._2).getOrElse {
      val (dists, temps) = distances.map {
        case (dist, temp) => (1 / pow(dist, p), temp / pow(dist, p))
      }.unzip
      temps.sum / dists.sum
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    points.find(_._1 == value).map(_._2).getOrElse {
      val (lowers, highers) = points.toList.sortBy(_._1).partition(_._1 < value)
      val lowest = lowers.reverse.headOption
      val highest = highers.headOption
      (lowest, highest) match {
        case (None, None) => Color(0, 0, 0)
        case (Some((_, color)), None) => color
        case (None, Some((_, color))) => color
        case (Some((lowTemp, lowColor)), Some((highTemp, highColor))) =>
          lowColor + (highColor - lowColor) * ((value - lowTemp) / (highTemp - lowTemp))
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width: Int = 360
    val height: Int = 180

    val points = (for {
      lat <- 90 until -90 by -1
      lon <- -180 until 180 by 1
    } yield Location(lat, lon)).par

    Image(width, height,
      points
        .map(loc => predictTemperature(temperatures, loc))
        .map(temp => interpolateColor(colors, temp))
        .map(color => Pixel(color.red, color.green, color.blue, 255))
        .toArray)
  }
}
