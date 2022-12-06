package dev.gabrielsson

class Day6 extends Inputs {

  def part1(input: Seq[String]): Int = puzzle(input, 4)


  def part2(input: Seq[String]): Int = puzzle(input, 14)

  private def puzzle(input: Seq[String], charUnique: Int) = input.head.sliding(charUnique)
    .foldLeft((0, false))((b, a) => b match {
      case t if t._2 => t
      case t if a.toSet.size == charUnique => (t._1 + charUnique, true)
      case t => (t._1 + 1, false)
    })._1

}
