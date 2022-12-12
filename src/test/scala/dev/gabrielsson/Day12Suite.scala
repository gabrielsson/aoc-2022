package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day12Suite extends AnyFlatSpec with Matchers {
  val day = new Day12

  "part1test" should "part1Test" in {
    day.part1(day.getTestRaw) shouldBe 31
  }
  "part1" should "part1" in {
    day.part1(day.getRaw) shouldBe 391
  }
  it should "part2Test" in {
    day.part2(day.getTestRaw) shouldBe 29
  }
  it should "part2" in {
    day.part2(day.getRaw) shouldBe 386
  }
}
