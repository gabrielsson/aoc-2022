package dev.gabrielsson

import scala.collection.mutable

class Day21 extends Inputs {

  val RootEx = """root: (.*) \+ (.*)""".r
  val NumberEx = """(.*): (\d+)""".r
  val PlusEx = """(.*): (.*) \+ (.*)""".r
  val MinusEx = """(.*): (.*) - (.*)""".r
  val DivEx = """(.*): (.*) / (.*)""".r
  val ProdEx = """(.*): (.*) \* (.*)""".r

  def part2(input: Seq[String], steps: Long = 10000000000L): Long = {
    val muteMap = mutable.Map[String, () => Long]()
    muteMap.addAll(input.map {
      case RootEx(id1, id2) => ("root", () => muteMap(id1).apply() - muteMap(id2).apply())
      case NumberEx(id, number) => (id, () => number.toLong)
      case PlusEx(id, id1, id2) => (id, () => muteMap(id1).apply() + muteMap(id2).apply())
      case MinusEx(id, id1, id2) => (id, () => muteMap(id1).apply() - muteMap(id2).apply())
      case DivEx(id, id1, id2) => (id, () => muteMap(id1).apply() / muteMap(id2).apply())
      case ProdEx(id, id1, id2) => (id, () => muteMap(id1).apply() * muteMap(id2).apply())
    })

    var adder = steps
    var humn = 0L
    muteMap("humn") = () => humn
    var neg = false
    
    Iterator.continually(muteMap("root").apply()).takeWhile(_ != 0).foreach(res => {
      if (res > 0) {
        if (neg) {
          adder = adder / 10
          neg = false
        }
        humn += adder
      } else {
        if (!neg) {
          adder = adder / 10
          neg = true
        }
        humn -= adder
      }
    })
    humn
  }

  def part1(input: Seq[String]): Long = {
    val muteMap = mutable.Map[String, () => Long]()
    muteMap.addAll(input.map {
      case NumberEx(id, number) => (id, () => number.toLong)
      case PlusEx(id, id1, id2) => (id, () => muteMap(id1).apply() + muteMap(id2).apply())
      case MinusEx(id, id1, id2) => (id, () => muteMap(id1).apply() - muteMap(id2).apply())
      case DivEx(id, id1, id2) => (id, () => muteMap(id1).apply() / muteMap(id2).apply())
      case ProdEx(id, id1, id2) => (id, () => muteMap(id1).apply() * muteMap(id2).apply())
    })
    muteMap("root").apply()
  }
}
