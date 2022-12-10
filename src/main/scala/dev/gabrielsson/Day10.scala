package dev.gabrielsson

class Day10 extends Inputs {
  private val AddX = """addx (-?\d+)""".r
  private val Noop = """noop""".r

  def part1(input: Seq[String]): Long = {
    input.foldLeft(Some(List[State](State(0, 1)), State(0, 1))) {
      case (Some((states, leftState)), instruction) =>
        instruction match {
          case AddX(v) => Some((states :+ leftState.inc :+ leftState.inc.inc, leftState.inc.inc.addx(v.toInt)))
          case Noop() => Some((states :+ leftState.inc, leftState.inc))
        }
    }
      .map {
        case (states: List[State], leftState: State) => (states :+ leftState).map(s => (s.clock -> s))
        case _ => Seq[(Long, State)]()
      }
      .map(l => l.toMap)
      .map(stateMap => {
        Seq.range(20, stateMap.size, 40)
          .map(stateMap(_))
          .map(_.signal)
          .sum
      }).getOrElse(-1L)
  }

  def part2(input: Seq[String]): Long = {
    input.foldLeft(Some(List[State](State(0, 1)), State(0, 1))) {
      case (Some((states, leftState)), instruction) =>
        instruction match {
          case AddX(v) => Some((states :+ leftState.inc :+ leftState.inc.inc, leftState.inc.inc.addx(v.toInt)))
          case Noop() => Some((states :+ leftState.inc, leftState.inc))
        }
    }
      .map {
        case (states: List[State], leftState: State) => (states :+ leftState.inc).map(s => (s.clock -> s.x))
      }
      .foreach(l => l.tail.grouped(40).foreach(l => {
        l.foreach(t => {
          if (t._2 - 1 to t._2 + 1 contains t._1 % 40 - 1) {
            print("#")
          } else {
            print(".")
          }
        })
        println()
      }))

    -1
  }

  case class State(clock: Long, x: Long) {
    def inc = State(clock + 1, x)

    def addx(v: Int) = State(clock, x + v)

    def signal: Long = clock * x
  }
}
