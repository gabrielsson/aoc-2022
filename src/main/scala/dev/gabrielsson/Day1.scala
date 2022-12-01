package dev.gabrielsson

class Day1 extends Day[Seq[Seq[String]], Int] {

  override def part1(input: Seq[Seq[String]]): Int = {
    input.map(_.map(_.toInt).sum)
      .max
  }

  override def part2(input: Seq[Seq[String]]): Int = {
    input.map(_.map(_.toInt).sum)
      .sorted.reverse.take(3).sum
  }
}
