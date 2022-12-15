package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day15Suite extends AnyFlatSpec with Matchers {
  val day = new Day15

  it should "part1Test" in {
    day.part1(day.getTestInput, 10) shouldBe 26
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 4737567
  }
  it should "part2Test" in {
    day.part2(day.getTestInput, 10) shouldBe -1
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe -1
  }
}
