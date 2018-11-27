package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {
  test("tileLocation"){
    assert(Interaction.calLocation(0, 0, 0) === Location(85.05112877980659, -180.0))
    assert(Interaction.calLocation(1, 1, 1) === Location(0.0, 0.0))
    assert(Interaction.calLocation(100, 100, 5) === Location(-89.99999212633796, 945.0))
  }
}
