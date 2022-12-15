package dev.gabrielsson

import dev.gabrielsson.GridExtensions.{Grid, GridCanvas}
import dev.gabrielsson.Points.{Box, Line, Point}

class Day15 extends Inputs {

  def part1(input: Seq[String], testY: Int = 2000000): Int = {

    val pointPairs: Seq[(Point, Point)] = input.map({
      case Beacon(x1, y1, x2, y2) => (Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    })

    val allPoints = pointPairs.flatMap(p => Seq(p._1, p._2))
    val maxy = allPoints.maxBy(_.y).y

    val miny = allPoints.minBy(_.y).y

    val sensors = pointPairs.map(_._1)
    val beacon = pointPairs.map(_._2)


    val yedges = pointPairs.map(t => (t._1, t._1 manhattan t._2)).map(
      kv => {
        val p = kv._1
        val m = kv._2

        Seq(Point(p.x, p.y - m), Point(p.x + m, p.y), Point(p.x, p.y + m), Point(p.x - m, p.y))
      }).flatMap(edges => {
      val l1 = Line(edges(0), edges(1))
      val l2 = Line(edges(1), edges(2))
      val l3 = Line(edges(2), edges(3))
      val l4 = Line(edges(3), edges(0))
      l1.points() ++ l2.points() ++ l3.points() ++ l4.points()
    }).filter(p => p.y == testY)

    val minx = yedges.minBy(_.x).x
    val maxx = yedges.maxBy(_.x).x
    val base = Line(Point(minx, testY), Point(maxx, testY)).points()
    val entire = base ++ sensors


    entire.filter(p => p.y == testY).count(p => !beacon.contains(p))

  }

  def part2(input: Seq[String], max: Int = 4000000): Int = {
    -1
  }

  val Beacon = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r

  private def printBeacon(minx: Int, maxx: Int, miny: Int, maxy: Int, beacons: Seq[Point], sensors: Seq[Point]) = {
    //'\u2588'
    val grid: Grid[Char] = Box(Point(minx - 1, 0), Point(maxx + 1, maxy + 1)).iterator.map(p => {
      val c: Char = if (beacons.contains(p)) 'B' else if (sensors.contains(p)) 'S' else '.'
      (p, c)
    }).toMap

    val canvas = grid.canvas(' ')(v => v)
    canvas.foreach(a => {
      a.slice(minx, maxx).foreach(print)
      print("\n")
    })
  }
}
