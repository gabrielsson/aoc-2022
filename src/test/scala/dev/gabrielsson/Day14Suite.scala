package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day14Suite extends AnyFlatSpec with Matchers {
  val day = new Day14

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 24
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 843
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 93
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 27625
  }
}
