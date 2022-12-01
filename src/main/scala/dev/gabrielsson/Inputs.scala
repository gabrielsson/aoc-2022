package dev.gabrielsson


import scala.collection.mutable
import scala.io.Source

trait Inputs {
  val fileName = s"${this.getClass.getTypeName.toLowerCase.replace("$", "").split("\\.").last}.txt"
  val testFileName = s"${this.getClass.getTypeName.toLowerCase.replace("$", "").split("\\.").last}test.txt"


  def getInput: Seq[String] = read(fileName)

  def getTestInput: Seq[String] = read(testFileName)

  def getNonSeparatedIntegers: Seq[Int] = getInput.head.split("\\B").map(_.toInt).toSeq

  def read(path: String): Seq[String] = {
    val source = Source.fromResource(path)
    try {
      source.getLines().toSeq
    } finally {
      source.close()
    }
  }

  def groups(strings: Seq[String]): Seq[Seq[String]] = {
    val groups = mutable.ListBuffer[Seq[String]]()
    val subList = new mutable.ListBuffer[String]
    for (string <- strings) {
      if (string.isBlank) {
        groups += subList.toSeq
        subList.clear()
      } else {
        subList.append(string)
      }
    }
    groups += subList.toSeq
    groups.toSeq
  }
}