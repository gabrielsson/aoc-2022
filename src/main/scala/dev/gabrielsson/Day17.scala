package dev.gabrielsson

import dev.gabrielsson.GridExtensions.{Grid, GridCanvas}
import dev.gabrielsson.Points.Point

import scala.collection.mutable

class Day17 extends Inputs {

  def part1(input: Seq[String]): Int = {
    var blocks: Int = 0
    val grid = mutable.HashMap[Point, Char]()
    val pushesChar: Iterator[Char] = Iterator.continually(input.head).flatten
    var currBlock = listOfBlocks(blocks % 5).moveBy(Point(2, -3))

    pushesChar.takeWhile(_ => blocks <= 2022).foreach(jet => {

      val down = Point(0, 1)
      val horizontal = jet match {
        case '<' => Point(-1, 0)
        case '>' => Point(1, 0)
      }

      if (currBlock.canMove(horizontal, grid.toMap)) {
        currBlock = currBlock.moveBy(horizontal)
      }

      if (currBlock.canMove(down, grid.toMap)) {
        currBlock = currBlock.moveBy(down)
      } else {
        currBlock.foreach(p => grid(p) = '#')
        val minY = grid.keys.minBy(_.y).y
        blocks += 1
        currBlock = listOfBlocks(blocks % 5).map(p => p + Point(2, minY - 4))
        grid.keys.filter(key => {
          key.y >  minY+50
        }).foreach(grid.remove)
      }
   })

    grid.keys.minBy(_.y).y.abs - 1
  }

  def part2(input: Seq[String]): Long = {
    var blocks: Int = 0
    var last: Int = 0
    val grid = mutable.HashMap[Point, Char]()
    val pushesChar: Iterator[Char] = Iterator.continually(input.head).flatten
    var currBlock = listOfBlocks(blocks % 5).moveBy(Point(2, -3))


    pushesChar.drop(blocks).takeWhile(_ => blocks <=100000)foreach(jet => {

      val down = Point(0, 1)
      val horizontal = jet match {
        case '<' => Point(-1, 0)
        case '>' => Point(1, 0)
      }

      if (currBlock.canMove(horizontal, grid.toMap)) {
        currBlock = currBlock.moveBy(horizontal)
      }

      if (currBlock.canMove(down, grid.toMap)) {
        currBlock = currBlock.moveBy(down)
      } else {
        currBlock.foreach(p => grid(p) = '#')
        val minY = grid.keys.minBy(_.y).y
        blocks += 1
        currBlock = listOfBlocks(blocks % 5).map(p => p + Point(2, minY - 4))
        grid.keys.filter(key => {
          key.y > minY + 50
        }).foreach(grid.remove)
        //test for repetition... started by printing every 1000 and see if it repeats
        if (blocks > 500 && blocks % 8700==0 || blocks == 26100 + 6400) {
          val i = grid.keys.minBy(_.y).y.abs
          println((i-last) + "@" + blocks)
          last = i
        }
      }
    })

    // first value, number of repititions, reptition value, rest  + 1
    13312L+(114942527L*13330L) + 9825 + 1

  }

  type Block = Seq[Point]

  implicit class BlockMove(val block: Block) {
    def moveBy(point: Point): Block = {
      block.map(p => p + point)
    }

    def canMove(point: Point, grid: Grid[Char]): Boolean = {
      val newBlock = moveBy(point)
      !newBlock.exists(p => grid.contains(p) || p.x < 0 || p.x > 6 || p.y > 0)
    }
  }


  val listOfBlocks = List(
    Seq(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)),
    Seq(Point(1, 0), Point(1, -1), Point(1, -2), Point(2, -1), Point(0, -1)),
    Seq(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, -1), Point(2, -2)),
    Seq(Point(0, 0), Point(0, -1), Point(0, -2), Point(0, -3)),
    Seq(Point(0, 0), Point(0, -1), Point(1, 0), Point(1, -1))
  )


  private def printGrid(grid: Grid[Char]) = {
    grid.canvas('.')(v => v).reverse.foreach(a => {
      a.foreach(print)
      print("\n")
    })
  }
}
