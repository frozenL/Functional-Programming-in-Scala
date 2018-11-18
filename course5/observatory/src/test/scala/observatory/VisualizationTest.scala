package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {
  lazy val visualization = Visualization

  test("exceeding the greatest value of a color scale should return the color associated with the greatest value") {
    val testColors = Seq((79.99746973742927,Color(255,0,0)), (100.0,Color(0,0,255)))
    assert(visualization.interpolateColor(testColors, 69.99746973742927) == Color(255, 0, 0))
  }

  test("predictTemperature"){
    val testTemperatures = Seq(
      (Location(32.95, 65.56), 15.00), (Location(58.43, 5.80), 25.00),
      (Location(41.65, 12.43), 10.00))

    println(visualization.predictTemperature(testTemperatures, Location(11.50, 23)))
    assert(visualization.predictTemperature(testTemperatures, Location(58.43, 5.8)) === 25.00)
  }

  test("interpolateColor"){
    val testPoints = Seq(
      (60.0, Color(255,255,255)), (32.0, Color(255,0,0)), (12.0, Color(255,255,0)),
      (0.0, Color(0,255,255)), (-15.0, Color(0,0,255)), (-27.0, Color(255,0,255)))

    assert(visualization.interpolateColor(testPoints, 32.0) === Color(255,0,0))
    assert(visualization.interpolateColor(testPoints, 15.0) ===
      Color(255, (math.round(-255.0*3.0/20.0) + 255.0).toInt,0))
  }
}
