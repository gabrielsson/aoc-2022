package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day18Suite extends AnyFlatSpec with Matchers {
  val day = new Day18

  it should "part1TestEasy" in {
    day.part1(Seq("1,1,1", "2,1,1")) shouldBe 10
  }
  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe 64
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe 4390
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe 58
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe 2534
  }
}
