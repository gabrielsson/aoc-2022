package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day9Suite extends AnyFlatSpec with Matchers {
  val day = new Day9

  behavior of "part1"
  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe -1
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe -1
  }
  behavior of "part2"
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe -1
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe -1
  }
}
