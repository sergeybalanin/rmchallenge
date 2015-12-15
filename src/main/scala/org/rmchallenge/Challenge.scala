package org.rmchallenge


object Challenge extends (Array[Array[Int]] => (Int, Int)) {

  type HeightMap = Position => Int
  type ValidArea = Position => Boolean

  trait DescendMap {

    def stepsFromPeak(at: Position): Int

    def update(at: Position, peakHeight: Int, steps: Int): Unit
  }

  final case class Position(x: Int, y: Int)
                           (implicit validArea: ValidArea, heightMap: HeightMap) {

    def up = Position(x, y - 1)

    def down = Position(x, y + 1)

    def left = Position(x - 1, y)

    def right = Position(x + 1, y)

    def hood: Traversable[Position] = List(up, down, left, right).filter(pos => validArea(pos))

    def descends: Traversable[Position] = hood.filter(near => heightMap(this) > heightMap(near))

    def findDescends(descendMap: DescendMap): Unit = findDescends(1, heightMap(this), descendMap)

    def findDescends(stepsFromPeak: Int, peakHeight: Int, descendMap: DescendMap): Unit =
      descendMap.stepsFromPeak(this) match {
        case steps if (steps < stepsFromPeak) || (steps == stepsFromPeak && steps != 0) =>
          descendMap.update(this, peakHeight, stepsFromPeak)
          descends.foreach(_.findDescends(stepsFromPeak + 1, peakHeight, descendMap))
        case otherwise => ()
      }
  }

  override def apply(input: Array[Array[Int]]): (Int, Int) = {
    val xLimit = input.length
    val yLimit = input(0).length
    implicit val validArea: ValidArea = (pos) =>
      pos.x >= 0 && pos.x < xLimit && pos.y >= 0 && pos.y < yLimit
    implicit val heightMap: HeightMap = (pos) => input(pos.x)(pos.y)

    object passedPositions extends DescendMap {
      private val lengthMatrix: Array[Array[Int]] = Array.fill[Int](xLimit, yLimit)(0)
      var longestDescend: Int = 0
      var highestDrop: Int = 0

      def stepsFromPeak(at: Position): Int = lengthMatrix(at.x)(at.y)

      def update(at: Position, peakHeight: Int, steps: Int): Unit = {
        lengthMatrix(at.x).update(at.y, steps)
        val drop = peakHeight - heightMap(at)
        if (longestDescend < steps) {
          longestDescend = steps
          highestDrop = drop
        } else if (longestDescend == steps) {
          highestDrop = Math.max(drop, highestDrop)
        }
      }
    }

    for (x <- 0 until xLimit; y <- 0 until yLimit) {
      Position(x, y).findDescends(descendMap = passedPositions)
    }
    (passedPositions.longestDescend, passedPositions.highestDrop)
  }
}
