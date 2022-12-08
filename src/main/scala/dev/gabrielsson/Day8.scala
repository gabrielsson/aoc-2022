package dev.gabrielsson

import dev.gabrielsson.GridExtensions.GridCharSeq
import dev.gabrielsson.Points.{Dir, Point, Position}

import scala.collection.mutable

class Day8 extends Inputs {

  def part1(input: String): Int = {
    val grid = input.toList.toIntGrid
    val maxX = grid.maxBy(_._1.x)._1.x
    val maxY = grid.maxBy(_._1.y)._1.y

    val left = grid.filter(_._1.x == 0).keys.toList.sortBy(_.y)
    val right = grid.filter(_._1.x == maxX).keys.toList.sortBy(_.y).reverse
    val top = grid.filter(_._1.y == 0).keys.toList.sortBy(_.x)
    val bottom = grid.filter(_._1.y == maxY).keys.toList.sortBy(_.x).reverse


    def take(startPoints: List[Point], incrementPoints: List[Point]): List[List[Point]] = {
      startPoints.tail.map(p => {
        val seenPoints = mutable.ListBuffer[Point]()
        incrementPoints.tail.map(i => p + i)
          .filter(p => p.x != 0 && p.y != 0 && p.x != startPoints.size - 1 && p.y != startPoints.size - 1)
          .map(d => {
            if (grid(d) > grid(p)) {
              if (seenPoints.isEmpty) {
                seenPoints.addOne(d)
              } else if (seenPoints.map(grid).max < grid(d)) {
                seenPoints.addOne(d)
              }
            }
          })
        seenPoints.toList
      })
    }

    val points = take(left, top) ++ take(right, top.map(_ * -1)) ++ take(top, left) ++ take(bottom, left.map(_ * -1))
    val seen = points.flatten.toSet

    val edges = (maxX + 1) * 2 + (maxY + 1) * 2 - 4
    seen.size + edges
  }


  def part2(input: String): Int = {
    val grid = input.toList.toIntGrid

    def collectView(start: Point): Int = {
      val viewList = Seq('U', 'D', 'L', 'R').map(c => {
        var found = false
        var dir = Dir(start, c).forward()
        var score = 0
        while (!found && grid.contains(dir.p)) {
          score += 1
          if (grid(dir.p) >= grid(start)) {
            found = true
          }
          dir = dir.forward()
        }
        score
      })
      viewList.product
    }

    grid.map(e => collectView(e._1)).max
  }
}
