package dev.gabrielsson

import scala.annotation.tailrec
import scala.collection.mutable

class Day11 extends Inputs {

  def round(m: Seq[Monkey], whoWillGetFunction: (Monkey, Long) => (String, Long)): Seq[Monkey] = {
    val monkeyItems = mutable.Map[String, Seq[Long]]()
    val inspectionCounts = mutable.Map[String, Long]()

    m.foreach(monkey => {
      val items = if (monkeyItems.contains(monkey.id)) {
        val is = monkey.items ++ monkeyItems(monkey.id)
        monkeyItems.update(monkey.id, Seq())
        is
      } else {
        monkey.items
      }
      items.foreach(item => {
        val (id, newItem) = whoWillGetFunction(monkey, item)
        monkeyItems.update(id, monkeyItems.getOrElse(id, Seq()) :+ newItem)
        inspectionCounts.update(monkey.id, inspectionCounts.getOrElse(monkey.id, 0L) + 1)

      })
    })
    m.map(m => m
      .copy(items = monkeyItems.getOrElse(m.id, Seq()))
      .copy(inspectionCount = m.inspectionCount + inspectionCounts.getOrElse(m.id, 0L))
    )
  }

  def part1(input: Seq[Seq[String]]): Long = {
    val monkeys = input.map(_.mkString("\n")).map(Monkey(_))

    val result = (1 to 20).foldLeft(monkeys)((m, i) => {
      val value = round(m, (monkey, item) => {
        val i = monkey.worryLevelFunction(item) / 3
        (if (monkey.test(i)) monkey.trueNext else monkey.falseNext, i)
      })
      if (i < 11 || i == 15 || i == 20) {
        println(s"After round $i, the monkeys are holding items with these worry levels:")
        value.foreach(m => println(s"Monkey ${m.id}: ${m.items.mkString(", ")}"))
        println()
      }

      value
    })

    result.sortBy(_.inspectionCount).reverse.head.inspectionCount *
      result.sortBy(_.inspectionCount).reverse.tail.head.inspectionCount

  }

  @tailrec
  private def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

  private def lcm(a: Long, b: Long) = (a * b).abs / gcd(a, b)

  def part2(input: Seq[Seq[String]]): Long = {
    val monkeys = input.map(_.mkString("\n")).map(Monkey(_))
    val leastCommonMultiple = monkeys.tail.foldLeft(monkeys.head.dividableBy)((l, monkey) => lcm(l, monkey.dividableBy))

    val result = (1 to 10000).foldLeft(monkeys)((m, i) => {
      val value = round(m.map(monkey => {
        monkey.copy(items = monkey.items.map(i => i % leastCommonMultiple))
      }), (monkey, item) => {
        val i = monkey.worryLevelFunction(item)
        (if (monkey.test(i)) monkey.trueNext else monkey.falseNext, i)
      })
      if (i == 1 || i == 20 || i % 1000 == 0) {
        println(s"== After round $i ==")
        value.foreach(m => {
          println(s"Monkey ${m.id} inspected items ${m.inspectionCount} times.")
        })
      }
      value
    })

    result.sortBy(_.inspectionCount).reverse.head.inspectionCount *
      result.sortBy(_.inspectionCount).reverse.tail.head.inspectionCount
  }

  case class Monkey(id: String, items: Seq[Long],
                    worryLevelFunction: Long => Long,
                    test: Long => Boolean,
                    trueNext: String,
                    falseNext: String,
                    inspectionCount: Long,
                    dividableBy: Long)

  object Monkey {
    private val MonkeyRegex =
      """Monkey (\d+):
        |  Starting items: (.*)
        |  Operation: new = old ([*|+]?) (.*)
        |  Test: divisible by (\d+)
        |    If true: throw to monkey (\d+)
        |    If false: throw to monkey (\d+)""".stripMargin.r

    def apply(string: String): Monkey = {
      string match {
        case MonkeyRegex(id, items, operator, operation, test, trueNext, falseNext) =>

          val worryLevelFunction: Long => Long = i => {
            val factor = if (operation == "old") i else operation.toLong
            if (operator == "*") {
              i * factor
            } else {
              i + factor
            }
          }

          val testFunc: Long => Boolean = i => i % test.toLong == 0

          Monkey(id,
            items.split(", ").map(_.toLong).toSeq,
            worryLevelFunction,
            testFunc,
            trueNext,
            falseNext,
            0,
            test.toLong)
      }
    }
  }
}
