package dev.gabrielsson

trait Day[T, R] extends Inputs{

  def part1(input: T): R

  def part2(input: T): R

}
