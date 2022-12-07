package dev.gabrielsson

import dev.gabrielsson.GridExtensions.{Grid, GridCanvas, GridCharSeq}
import dev.gabrielsson.Points.Point
import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GraphsTests extends AnyFlatSpec with Matchers {


  it should "add" in {
    Point(0, 0) + Point(1, 2) shouldBe Point(1, 2)
  }
  it should "manhattan" in {
    Point(4, 0) manhattan Point(0, 4) shouldBe 8
  }
  it should "surrounding" in {
    Point(0, 0).surroundings should contain allOf(Point(-1, 0), Point(-1, -1), Point(-1, 1))
  }

  it should "create grid" in {
    val grid = "122\n123\n111".toList.toIntGrid
    grid.size shouldBe 9
  }

  it should "astar" in {
    val grid =
      """1991
        |2991
        |2222""".stripMargin.toList.toIntGrid
    val heuristicFunction: Point => Int = {
      p => p.surroundings.filter(grid.contains).map(s => grid(s)).sum
    }
    val neighborsFunction: Point => Iterable[Point] = {
      p => p.neighbors.filter(n => grid.contains(n))
    }
    val costOfTransportingFromAtoB: (Point, Point) => Int = (a, b) => {
      grid(b)
    }

    Graphs.aStar(Point.zero, Point(3, 2))(costOfTransportingFromAtoB)(neighborsFunction)(heuristicFunction)._2.get._2 shouldBe 10
  }

  it should "dijkstra" in {
    val grid =
      """1991
        |2991
        |2222""".stripMargin.toList.toIntGrid
    val neighborsFunction: Point => Iterable[Point] = {
      p => p.neighbors.filter(n => grid.contains(n))
    }
    val costOfTransportingFromAtoB: (Point, Point) => Int = (a, b) => {
      grid(b)
    }

    Graphs.dijkstra(Point.zero, Point(3, 2))(neighborsFunction)(costOfTransportingFromAtoB)._2.get._2 shouldBe 10
  }

  it should "bfs" in {
    val grid =
      """.###
        |..##
        |#...""".stripMargin.toList.toGrid
    val neighborsFunction: Point => Iterable[Point] = {
      p => p.neighbors.filter(n => grid.contains(n)).filter(n => grid(n) == '.')
    }

    val path = Graphs.bfs(Point.zero)(neighborsFunction).map(t => (t._2, t._1))
    path(0) shouldBe Point(0,0)
    path(1) shouldBe Point(0,1)
    path(2) shouldBe Point(1,1)
    path(3) shouldBe Point(1,2)
    path(4) shouldBe Point(2,2)
    path(5) shouldBe Point(3,2)
  }

  it should "dfs" in {
    val grid =
      """.###
        |..##
        |#...""".stripMargin.toList.toGrid
    val neighborsFunction: Point => Iterable[Point] = {
      p => p.neighbors.filter(n => grid.contains(n)).filter(n => grid(n) != '#')
    }

    val path = Graphs.dfs(Point.zero)(neighborsFunction).toList.reverse
    path.head shouldBe Point(0, 0)
    path(1) shouldBe Point(0, 1)
    path(2) shouldBe Point(1, 1)
    path(3) shouldBe Point(1, 2)
    path(4) shouldBe Point(2, 2)
    path(5) shouldBe Point(3, 2)
  }

  it should "print canvas" in {
    val grid: Grid[Char] =
      """.###
        |..##
        |#...""".stripMargin.toList.toGrid


    println(grid.canvas(' ')(v => v).foreach(a => {
      a.foreach(print)
      println
    }))
  }
}
