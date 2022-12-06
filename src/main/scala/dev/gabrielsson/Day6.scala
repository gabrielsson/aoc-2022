package dev.gabrielsson

class Day6 extends Inputs {

  def part1(input: Seq[String]): Int = puzzle2(input, 4)


  def part2(input: Seq[String]): Int = puzzle2(input, 14)

  private def puzzle2(input: Seq[String], charUnique: Int) =
    input.head.sliding(charUnique)
    .takeWhile(_.toSet.size != charUnique).size + charUnique

}
