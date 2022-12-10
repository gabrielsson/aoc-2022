package dev.gabrielsson

import dev.gabrielsson.GridExtensions.{Grid, GridCanvas, GridCharSeq}
import dev.gabrielsson.Points.{Box, Dir, Point, Position}

import scala.collection.mutable

class Day9 extends Inputs {

  def part1(input: Seq[String]): Int = {
    var head = Position.zero
    var tail = Position.zero
    var trail = mutable.ListBuffer[Point]()
    input.map(_.split(" ")).foreach(step => {
      val direction = step(0).charAt(0)
      val n = step(1).toInt
      trail.addOne(tail.copy())

      val dir = Dir(head, direction)

      (1 to n).foreach(i => {
        head = dir.forward(i).p

        if (!head.surroundings.contains(tail) && head != tail) {
          if (head.x == tail.x) {
            val tailDir = Dir(tail, 'U')
            val steps = if (tail.y - head.y < 0) -1 else 1

            tail = tailDir.forward(steps).p
          } else if (head.y == tail.y) {
            val tailDir = Dir(tail, 'R')
            val steps = if (head.x - tail.x < 0) -1 else 1
            tail = tailDir.forward(steps).p
          } else {
            val neighbors = head.neighbors
            val point = neighbors.filter(p => (p.x - tail.x).abs <= 1 && (p.y - tail.y).abs <= 1).head
            tail = point
          }
          trail.addOne(tail.copy())
        }


      })
      trail.addOne(tail.copy())


    })


    println(trail.distinct)
    trail.toSet.size
  }

  def part2(input: Seq[String]): Int = {

    var rope = mutable.ListBuffer[Point]()

    rope.addAll((1 to 10).map(_ => Point(400,400)))
    var trail = mutable.ListBuffer[Point]()


    def move(headi: Int, taili: Int) = {

      val head = rope(headi)
      val tail = rope(taili)
      if (!head.surroundings.contains(tail) && head != tail) {
        if (headi == 0) {
          if (head.x == tail.x) {
            val tailDir = Dir(tail, 'U')
            val steps = if (tail.y - head.y < 0) -1 else 1
            rope.update(taili, tailDir.forward(steps).p)
          } else if (head.y == tail.y) {
            val tailDir = Dir(tail, 'R')
            val steps = if (head.x - tail.x < 0) -1 else 1
            rope.update(taili, tailDir.forward(steps).p)
          } else {
            val neighbors = head.neighbors
            val point = neighbors.filter(p => (p.x - tail.x).abs <= 1 && (p.y - tail.y).abs <= 1).head
            rope.update(taili, point)
          }
          trail.addOne(rope.last)
        } else {
          val neighbors = head.surroundings
          val point = neighbors.filter(p => (p.x - tail.x).abs <= 1 && (p.y - tail.y).abs <= 1).head
          rope.update(taili, point)
          trail.addOne(rope.last)
        }
      }
    }

    input.map(_.split(" ")).foreach(step => {
      val direction = step(0).charAt(0)
      val n = step(1).toInt
      trail.addOne(rope.last)


      (1 to n).foreach(i => {
        val dir = Dir(rope.head, direction)
        rope(0) = dir.forward().p
        (0 to 8).foreach(j => {
          move(j, j + 1)
        })
      })
    })


    val set = trail.toIndexedSeq.toSet
    val iterator = Box(Point(set.map(_.x).min, set.map(_.y).min), Point(set.map(_.x).max + 10, set.map(_.y).max + 10)).iterator

    val grid: Grid[Char] = iterator.map(p => (p, if (set.contains(p)) '#' else '.')).toMap

    grid.canvas('.')(v => v).foreach(a => {
      a.foreach(print)
      print("\n")
    })
    set.size
  }


}
