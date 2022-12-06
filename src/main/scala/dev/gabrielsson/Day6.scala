package dev.gabrielsson

class Day6 extends Inputs {

  def part1(input: Seq[String]): Int = process(input, 4)
  
  def part2(input: Seq[String]): Int = process(input, 14)

  private def process(input: Seq[String], noOfChars: Int) =
    input.head.sliding(noOfChars)
      .takeWhile(_.distinct.length != noOfChars)
      .size + noOfChars

}
