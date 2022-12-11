package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day11Suite extends AnyFlatSpec with Matchers {
  val day = new Day11

  it should "part1Test" in {
    day.part1(day.groups[String](day.getTestInput, s => s.isBlank)) shouldBe 10605L
  }
  it should "part1" in {
    day.part1(day.groups[String](day.getInput, s => s.isBlank)) shouldBe 51075L
  }
  it should "part2Test" in {
    day.part2(day.groups[String](day.getTestInput, s => s.isBlank)) shouldBe 2713310158L
  }
  it should "part2" in {
    day.part2(day.groups[String](day.getInput, s => s.isBlank)) shouldBe 11741456163L
  }
}
