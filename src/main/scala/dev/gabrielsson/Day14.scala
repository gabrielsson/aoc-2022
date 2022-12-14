package dev.gabrielsson

import dev.gabrielsson.GridExtensions.{Grid, GridCanvas}
import dev.gabrielsson.Points.{Box, Dir, Line, Point}

import scala.collection.mutable

class Day14 extends Inputs {
  private val PointRegex = """(\d+),(\d+)""".r

  private def parsePoint(s1: String) = {
    s1 match {
      case PointRegex(x, y) => Point(x.toInt, y.toInt)
    }
  }

  def part1(input: Seq[String]): Int = {
    val lines = input.flatMap(s => s.split(" -> ").sliding(2).map(e => Line(parsePoint(e(0)), parsePoint(e(1))))).toSet

    //println(lines)

    val linePoints = lines.flatMap(_.points())
    val minx = linePoints.minBy(_.x).x
    val maxx = linePoints.maxBy(_.x).x
    val maxy = linePoints.maxBy(_.y).y


    val blockedPoints = mutable.Set[Point]()
    blockedPoints.addAll(linePoints)

    var iteration = 0
    var found = false
    while (!found) {
      iteration += 1
      var rest = false
      var sand = Dir(Point(500, 0), 'D')
      while (!rest) {
        if (blockedPoints.contains(sand.forward().p)) {
          val leftDir = Dir(sand.p.copy(x = sand.p.x - 1), sand.dir)
          val rightDir = Dir(sand.p.copy(x = sand.p.x + 1), sand.dir)
          if (!blockedPoints.contains(leftDir.forward().p)) {

            sand = leftDir.forward()
          } else if (!blockedPoints.contains(rightDir.forward().p))
            sand = rightDir.forward()
          else {
            blockedPoints.addOne(sand.p)
            rest = true
          }
        } else {
          rest = sand.p.y > maxy
          if (!rest)
            sand = sand.forward()

        }
      }

      found = sand.p.y > maxy

    }
    printSand(maxy, minx, maxx, linePoints, blockedPoints.toSet)
    iteration - 1
  }

  def part2(input: Seq[String]): Int = {
    val lines = input.flatMap(s => s.split(" -> ").sliding(2).map(e => Line(parsePoint(e(0)), parsePoint(e(1))))).toSet

    val walls = lines.flatMap(_.points())
    val maxy = walls.maxBy(_.y).y + 2
    val minx = walls.minBy(_.x).x - maxy
    val maxx = walls.maxBy(_.x).x + maxy

    val infLine = Line(Point(minx, maxy), Point(maxx, maxy)).points()
    val linePoints = walls ++ infLine
    val blockedPoints = mutable.Set[Point]()
    blockedPoints.addAll(linePoints)
    blockedPoints.addAll(infLine)

    var iteration = 0
    var found = false
    while (!found) {
      iteration += 1
      var rest = false
      var sand = Dir(Point(500, 0), 'D')
      while (!rest) {
        if (blockedPoints.contains(sand.forward().p)) {
          val leftDir = Dir(sand.p.copy(x = sand.p.x - 1), sand.dir)
          val rightDir = Dir(sand.p.copy(x = sand.p.x + 1), sand.dir)
          if (!blockedPoints.contains(leftDir.forward().p)) {

            sand = leftDir.forward()
          } else if (!blockedPoints.contains(rightDir.forward().p))
            sand = rightDir.forward()
          else {
            blockedPoints.addOne(sand.p)
            rest = true
          }
        } else {
          rest = sand.p.y > maxy
          if (!rest)
            sand = sand.forward()

        }
      }

      found = sand.p.y > maxy || blockedPoints.contains(Point(500, 0))
    }

    printSand(maxy, minx, maxx, linePoints, blockedPoints.toSet)
    iteration
  }

  private def printSand(maxy: Int, minx: Int, maxx: Int, linePoints: Set[Point], blockedPoints: Set[Point]) = {
    val grid: Grid[Char] = Box(Point(minx - 1, 0), Point(maxx + 1, maxy + 1)).iterator.map(p => {
      val wall = if (linePoints.contains(p)) '\u2588' else if (blockedPoints.contains(p)) 'o' else ' '
      (p, wall)
    }).toMap

    val canvas = grid.canvas(' ')(v => v)
    canvas.foreach(a => {
      a.slice(minx, maxx).foreach(print)
      print("\n")
    })
  }
}
