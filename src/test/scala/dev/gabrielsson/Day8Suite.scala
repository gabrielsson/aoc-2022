package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day8Suite extends AnyFlatSpec with Matchers {
  val day = new Day8


  "part1test" should "part1Test" in {
    day.part1(day.getTestRaw) shouldBe 21
  }
  "part1" should "part1" in {
    day.part1(day.getRaw) shouldBe 1662
  }
  it should "part2Test" in {
    day.part2(day.getTestRaw) shouldBe 8
  }
  it should "part2" in {
    day.part2(day.getRaw) shouldBe 537600
  }
}
