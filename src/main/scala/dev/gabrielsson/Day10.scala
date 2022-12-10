package dev.gabrielsson

class Day10 extends Inputs {
  private val AddX = """addx (-?\d+)""".r
  private val Noop = """noop""".r

  def part1(input: Seq[String]): Long = {
    val res = input.foldLeft((List[State](State(0, 1)), State(0, 1)))((s, instruction) => {
      val states = s._1
      val leftState = s._2
      instruction match {
        case AddX(v) => (states :+ leftState.inc :+ leftState.inc.inc, leftState.inc.inc.addx(v.toInt))
        case Noop() => (states :+ leftState.inc, leftState.inc)
      }
    })
    val stateMap = (res._1 :+ res._2.copy(clock = res._2.clock + 1))
      .map(s => (s.clock, s))
      .toMap

    Seq.range(20, stateMap.size, 40)
      .map(stateMap(_))
      .map(_.signal)
      .sum
  }

  def part2(input: Seq[String]): Long = {
    val res = input.foldLeft((List[State](State(0, 1)), State(0, 1)))((s, instruction) => {
      val states = s._1
      val leftState = s._2
      instruction match {
        case AddX(v) => (states :+ leftState.inc :+ leftState.inc.inc, leftState.inc.inc.addx(v.toInt))
        case Noop() => (states :+ leftState.inc, leftState.inc)
      }
    })
    (res._1 :+ res._2.copy(clock = res._2.clock + 1))
      .map(s => (s.clock, s.x)).sortBy(_._1)
      .tail.grouped(40).foreach(l => {
      l.foreach(t => {
        if (t._2 - 1 to t._2 + 1 contains t._1 % 40 - 1) {
          print("#")
        } else {
          print(".")
        }
      })
      println()
    })
    -1
  }

  case class State(clock: Long, x: Long) {
    def inc = State(clock + 1, x)

    def addx(v: Int) = State(clock, x + v)

    def signal: Long = clock * x
  }
}
