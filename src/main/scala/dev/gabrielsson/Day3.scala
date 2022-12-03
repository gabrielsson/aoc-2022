package dev.gabrielsson

class Day3 extends Inputs {

  def part1(input: Seq[String]): Int = {
    input.map(s => s.splitAt(s.length / 2))
      .map(t => t._1 intersect t._2)
      .map(_.head)
      .map(1 + "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".indexOf(_))
      .sum
  }

  def part2(input: Seq[String]): Int = {
    input.grouped(3)
      .map(g => g.head intersect g(1) intersect g(2))
      .map(_.head)
      .map(1 + "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".indexOf(_))
      .sum
  }
}
