package dev.gabrielsson

import scala.collection.mutable

class Day5 extends Inputs {
  case class op(moves: Int, from: Int, to: Int)

  def part1(input: Seq[String]): String = {
    val piles = mutable.Map(1 -> List("N", "W", "B"),
      2 -> List("B", "M", "D", "T", "P", "S", "Z", "L"),
      3 -> List("R", "W", "Z", "H", "Q"),
      4 -> List("R", "Z", "J", "V", "D", "W"),
      5 -> List("B", "M", "H", "S"),
      6 -> List("B", "P", "V", "H", "J", "N", "G", "L"),
      7 -> List("S", "L", "D", "H", "F", "Z", "Q", "J"),
      8 -> List("B", "Q", "G", "J", "F", "S", "W"),
      9 -> List("J", "D", "C", "S", "M", "W", "Z"))

    val ops: Seq[op] = input.map(i => {
      val s = i.replaceAll("move ", "").split("from")
      val moves = s(0).trim.toInt
      val a = s(1).split("to ")
      val from = a(0).trim.toInt
      val to = a(1).trim.toInt
      op(moves, from, to)
    })

    ops.foreach(op => {
      val c = piles(op.from).slice(0, op.moves)
      piles(op.from) = piles(op.from).slice(op.moves, piles(op.from).size)
      piles(op.to) = c.reverse ++ piles(op.to)
    })

    piles.map(a => a._2.head).mkString


  }

  def part2(input: Seq[String]): String = {
    val piles = mutable.Map(1 -> List("N", "W", "B"),
      2 -> List("B", "M", "D", "T", "P", "S", "Z", "L"),
      3 -> List("R", "W", "Z", "H", "Q"),
      4 -> List("R", "Z", "J", "V", "D", "W"),
      5 -> List("B", "M", "H", "S"),
      6 -> List("B", "P", "V", "H", "J", "N", "G", "L"),
      7 -> List("S", "L", "D", "H", "F", "Z", "Q", "J"),
      8 -> List("B", "Q", "G", "J", "F", "S", "W"),
      9 -> List("J", "D", "C", "S", "M", "W", "Z"))

    val ops: Seq[op] = input.map(i => {
      val s = i.replaceAll("move ", "").split("from")
      val moves = s(0).trim.toInt
      val a = s(1).split("to ")
      val from = a(0).trim.toInt
      val to = a(1).trim.toInt
      op(moves, from, to)
    })

    ops.foreach(op => {
      val c = piles(op.from).slice(0, op.moves)
      piles(op.from) = piles(op.from).slice(op.moves, piles(op.from).size)
      piles(op.to) = c ++ piles(op.to)


    })

    piles.map(a => a._2.head).mkString
  }
}
