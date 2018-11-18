package observatory

import scala.math._

/**
  * Introduced in Week 1. Represents a location on the globe.
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  /**
    * Assuming that the radius is 1
    * @param to The Location of the other point
    * @return The degree difference between two points
    */
  def distance (to: Location): Double = {
    val radLat = toRadians(lat)
    val radLon = toRadians(lon)
    val radLatTo = toRadians(to.lat)
    val radLonTo = toRadians(to.lon)
    acos(sin(radLat) * sin(radLatTo) + cos(radLat) * cos(radLatTo) * cos(abs(radLon - radLonTo)))
  }
}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int)

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int)

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int) {
  def + (that: Color): Color = Color(red + that.red, green + that.green, blue + that.blue)
  def - (that: Color): Color = Color(red - that.red, green - that.green, blue - that.blue)
  def * (value: Double): Color = Color(round(red * value).toInt, round(green * value).toInt, round(blue * value).toInt)
}

