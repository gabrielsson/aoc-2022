package dev.gabrielsson

class Day3 extends Inputs {

  def part1(input: Seq[String]): Int = {
    input.map(s => (s.substring(0, s.length/2), s.substring(s.length/2)))
      .map(t => t._1.filter(c => t._2.contains(c)))
      .map(_.toCharArray.head)
      .map(1 + "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".indexOf(_))
      .sum
  }

  def part2(input: Seq[String]): Int = {
    input.grouped(3)
      .map(g => g.head.filter(s => g(1).contains(s)).filter(s => g(2).contains(s)))
      .map(_.toCharArray.head)
      .map(1 + "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".indexOf(_))
      .sum
  }
}
