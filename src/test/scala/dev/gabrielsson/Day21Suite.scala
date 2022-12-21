package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day21Suite extends AnyFlatSpec with Matchers {
  val day = new Day21

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 152L
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 72664227897438L
  }
  it should "part2Test" in {
    day.part2(day.getTestInput, -10) shouldBe 301
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 3916491093817L
  }
}
