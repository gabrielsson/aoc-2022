package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day1Suite extends AnyFlatSpec with Matchers {
  val day = new Day1

  it should "part1Test" in {
    day.part1(day.groups[String](day.getTestInput, s=>s.isBlank)) shouldBe 24000
  }
  it should "part1" in {
    day.part1(day.groups[String](day.getInput, s=>s.isBlank)) shouldBe 70764
  }
  it should "part2Test" in {
    day.part2(day.groups[String](day.getTestInput, s=>s.isBlank)) shouldBe 45000
  }
  it should "part2" in {
    day.part2(day.groups[String](day.getInput, s=>s.isBlank )) shouldBe 203905
  }
}
