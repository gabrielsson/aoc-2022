package dev.gabrielsson

class Day4 extends Inputs {

  def part1(input: Seq[String]): Int = {
    input.map(s => s.split(","))
      .map(a => (toSeq(a(0)), toSeq(a(1))))
      .count { case (first, second) =>
        first.containsSlice(second) || second.containsSlice(first)
      }
  }

  def part2(input: Seq[String]): Int = {
    input.map(s => s.split(","))
      .map(a => (toSeq(a(0)), toSeq(a(1))))
      .count { case (first, second) =>
        (first intersect second).nonEmpty
      }
  }

  //"1-3" -> Seq(1,2,3)
  private def toSeq(s: String): Seq[Int] = {
    val t = s.split("-")
    t(0).toInt to t(1).toInt
  }
}
