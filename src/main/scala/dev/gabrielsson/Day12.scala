package dev.gabrielsson

import dev.gabrielsson.GridExtensions.GridCharSeq
import dev.gabrielsson.Points.{Point, Position}

class Day12 extends Inputs {

  def part1(input: String): Int = {
    val grid = input.toList.toGrid

    val start = grid.filter(_._2 == 'S').head._1
    val end = grid.filter(_._2 == 'E').head._1

    val costFunction = (p1: Point, p2: Point) => 1
    val heuristicFunction: Point => Int = _ manhattan end
    val neighborsFunction = (p: Point) => {
      val startVal = if (grid(p) == 'S') 'a' else grid(p)
      p.neighbors.filter(n => grid.contains(n)).filter(n => {
        val endVal = if (grid(n) == 'E') 'z' + 1 else grid(n)
        endVal - startVal <= 1
      })
    }

    val res = Graphs.aStar(start, end)(costFunction)(neighborsFunction)(heuristicFunction)
    res._2.get._2
  }

  def part2(input: String): Int = {
    val grid = input.toList.toGrid

    val starts = grid.filter(_._2 == 'a').map(_._1)
    val end = grid.filter(_._2 == 'E').head._1

    val costFunction = (p1: Point, p2: Point) => 1
    val heuristicFunction: Point => Int = _ manhattan end
    val neighborsFunction = (p: Point) => {
      val startVal = if (grid(p) == 'S') 'a' else grid(p)
      p.neighbors.filter(n => grid.contains(n)).filter(n => {
        val endVal = if (grid(n) == 'E') 'z' + 1 else grid(n)
        endVal - startVal <= 1
      })
    }


    starts.flatMap(start => {
      val res = Graphs.aStar(start, end)(costFunction)(neighborsFunction)(heuristicFunction)
      res._2.map(_._2)
    }).min
  }
}
