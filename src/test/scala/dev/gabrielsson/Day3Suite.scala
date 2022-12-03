package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day3Suite extends AnyFlatSpec with Matchers {
  val day = new Day3

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 157
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 8018
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 70
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 2518
  }
}
