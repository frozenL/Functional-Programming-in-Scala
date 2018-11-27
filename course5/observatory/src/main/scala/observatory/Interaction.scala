package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = calLocation(tile.x, tile.y, tile.zoom)

  def calLocation(x: Double, y: Double, zoom: Int): Location = {
    Location(math.atan(math.sinh(math.Pi - y / (1 << zoom) * 2 * math.Pi)) * 180.0 / math.Pi, x / (1 << zoom) * 360.0 - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val w = 256
    val h = 256
    val pixels = (0 until w * h).par.map(i => {
      val (x, y) = ((i % w).toDouble / w + tile.x, (i / w).toDouble / h + tile.y)
      val location = calLocation(x, y, tile.zoom + 1)
      val color = Visualization.interpolateColor(colors,
        Visualization.predictTemperature(temperatures, location))
      Pixel.apply(color.red, color.green, color.blue, 127)
    }).toArray
    Image.apply(w, h, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
    } generateImage(year, Tile(x, y, zoom), data)
  }
}
