package dev.gabrielsson

import dev.gabrielsson.Points3D.Point3d

class Day18 extends Inputs {

  def part1(input: Seq[String]): Int = {
    val value = input.map(s => {
      val ints: Array[Int] = s.split(",").map(_.toInt)
      Points3D.Point3d(ints.head, ints.tail.head, ints.tail.tail.head)
    })
    value.map(p => p.neighbors.count(s => !value.contains(s))).sum
  }

  def part2(input: Seq[String]): Int = {
    val allOccupiedPoints = input.map(s => {
      val ints: Array[Int] = s.split(",").map(_.toInt)
      Points3D.Point3d(ints.head, ints.tail.head, ints.tail.tail.head)
    })

    val maxX = allOccupiedPoints.maxBy(_.x).x
    val maxY = allOccupiedPoints.maxBy(_.y).y
    val maxZ = allOccupiedPoints.maxBy(_.z).z
    val minZ = allOccupiedPoints.minBy(_.z)

    val neighborsFunction: Point3d => Iterable[Point3d] = {
      p => {
        p.neighbors.filter(p => {
          p.x >= 0 && p.x <= maxX  &&
            p.y >= 0 && p.y <= maxY  &&
            p.z >= 0 && p.z <= maxZ
        }).filter(!allOccupiedPoints.contains(_))
      }
    }

    val path = Graphs.bfs(Point3d(0,0,minZ.z))(neighborsFunction).keys

    val all = for {
      x <- (0 to maxX)
      y <- (0 to maxY)
      z <- (0 to maxZ)
    } yield Point3d(x, y, z)
    val points = all diff path.toList
    points.map(p => p.neighbors.count(s => !points.contains(s))).sum
  }
}
