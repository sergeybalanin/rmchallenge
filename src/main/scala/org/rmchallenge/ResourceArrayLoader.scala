package org.rmchallenge

import scala.io.Source

object ResourceArrayLoader extends (String => Array[Array[Int]]) {

  override def apply(resourceName: String): Array[Array[Int]] = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream(resourceName))
      .getLines().filter(_.nonEmpty)
    val ySize: Int = lines.next().split("\\ ").map(_.toInt).apply(1)
    (for (i <- 0 until ySize) yield lines.next().split("\\ ").map(_.toInt)).toArray
  }
}
