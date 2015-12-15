package org.rmchallenge

import org.rmchallenge.Challenge.{DescendMap, Position}
import org.specs2.mutable.Specification

class ChallengeTest extends Specification {

  "Position hood" should {
    implicit val validArea = (pos: Position) =>
      pos.x >= 0 && pos.x < 5 && pos.y >= 0 && pos.y < 8
    implicit val heightMap = (at: Position) => at.x * at.y

    "include all four neighbours when centered" in {
      Position(3, 6).hood.toSet must be equalTo Set(
        Position(3, 5), Position(3, 7), Position(2, 6), Position(4, 6)
      )
    }

    "exclude out-of-border values" in {
      Position(0, 0).hood.toSet must be equalTo Set(Position(0, 1), Position(1, 0))
    }

    "populate descending ways" in {
      Position(3, 3).descends.toSet must be equalTo Set(Position(2, 3), Position(3, 2))
    }
  }

  "Position" should {

    "take 3 steps to descend from 6-5-4" in {
      implicit val validArea = (pos: Position) => pos.x == 0 && pos.y >= 0 && pos.y <= 2
      implicit val heightMap = (at: Position) => at.y match {
        case 0 => 4
        case 1 => 5
        case 2 => 6
      }

      object descendMap extends DescendMap {
        val matrix: Array[Int] = Array.fill[Int](3)(0)

        override def stepsFromPeak(at: Position): Int = {
          require(at.x == 0)
          matrix(at.y)
        }

        override def update(at: Position, peakHeight: Int, steps: Int): Unit = {
          require(at.x == 0)
          matrix.update(at.y, steps)
        }
      }
      Position(0, 0).findDescends(descendMap)
      Position(0, 1).findDescends(descendMap)
      Position(0, 2).findDescends(descendMap)
      descendMap.stepsFromPeak(at = Position(0, 0)) must be equalTo 3
      descendMap.stepsFromPeak(at = Position(0, 1)) must be equalTo 2
      descendMap.stepsFromPeak(at = Position(0, 2)) must be equalTo 1
    }
  }

  "Challenge class" should {

    "return correct result for the given sample" in {
      Challenge(ResourceArrayLoader("/sample.txt")) must be equalTo(5, 8)
    }

    "find the email address" in {
      val solution = Challenge(ResourceArrayLoader("/map.txt"))
      val email = s"${solution._1}${solution._2}@redmart.com"
      println(email)
      success
    }
  }
}
