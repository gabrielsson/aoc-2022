package dev.gabrielsson

class Day2 extends Inputs {
  def part1(input: Seq[String]): Int = {
    input.map(_.split(" "))
      .map(a => (parse1(a(0)), parse1(a(1))))
      .map(t => calc(t._1, t._2))
      .sum
  }

  def part2(input: Seq[String]): Int = {
    input.map(_.split(" "))
      .map(a => parse2(a(0), a(1)))
      .map(t => calc(t._1, t._2))
      .sum
  }
  def calc(opp: Int, you: Int): Int = {
    if (opp == you) {
      3 + you
    } else if (you == winning(opp)) {
      6 + you
    } else {
      0 + you
    }
  }

  def winning(int: Int): Int = int match {
    case i if i > 2 => 1
    case i => i + 1
  }

  def lose(int: Int): Int = int match {
    case i if i < 2 => 3
    case i => i - 1
  }

  def parse1(str: String): Int = str match {
    case "A" => 1
    case "B" => 2
    case "C" => 3
    case "X" => 1
    case "Y" => 2
    case "Z" => 3
  }


  def parse2(opp: String, you: String): (Int, Int) = {
    val oi = opp match {
        case "A" => 1
        case "B" => 2
        case "C" => 3
      }
    val yi = you match {
      case "X" => lose(oi)
      case "Y" => oi
      case "Z" => winning(oi)
    }

    (oi, yi)
  }
}
