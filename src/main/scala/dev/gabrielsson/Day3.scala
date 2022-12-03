package dev.gabrielsson

class Day3 extends Inputs {

  def part1(input: Seq[String]): Int = {
    input.map(s => s.splitAt(s.length / 2))
      .map(t => t._1 intersect t._2)
      .map(_.head)
      .map(1 + ('a' to 'z').++('A' to 'Z').indexOf(_))
      .sum
  }

  def part2(input: Seq[String]): Int = {
    input.grouped(3)
      .map(g => g.tail.fold(g.head)((r, v) => v intersect r))
      .map(_.head)
      .map(1 + ('a' to 'z').++('A' to 'Z').indexOf(_))
      .sum
  }
}
