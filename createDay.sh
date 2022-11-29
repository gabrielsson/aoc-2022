cat << EOF > src/main/scala/dev/gabrielsson/Day$1.scala
package dev.gabrielsson

class Day$1 extends Day[Seq[String], Int] {

  override def part1(input: Seq[String]): Int = {
    -1
  }

  override def part2(input: Seq[String]): Int = {
    -1
  }
}
EOF
mkdir -p src/test/scala/dev/gabrielsson
cat << EOF > src/test/scala/dev/gabrielsson/Day$1Suite.scala
package dev.gabrielsson

import org.junit.runner.RunWith
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day1Suite extends AnyFlatSpec with Matchers {
  val day = new Day1

  it should "part1Test" in {
    day.part1(day.getTestInput) shouldBe -1
  }
  it should "part1" in {
    day.part1(day.getInput) shouldBe -1
  }
  it should "part2Test" in {
    day.part2(day.getTestInput) shouldBe -1
  }
  it should "part2" in {
    day.part2(day.getInput) shouldBe -1
  }
}
EOF
touch src/test/resources/day$1.txt
touch src/test/resources/day$1test.txt