package dev.gabrielsson

import dev.gabrielsson.Points.Point

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day7 extends Inputs {




  def part1(input: Seq[String]): Long = {
    val Cd = """cd (\w+)""".r

    var fileSystem = mutable.Map[String, AnyRef]()
    fileSystem.put("/", mutable.Map[String, AnyRef]())
    var buffer = mutable.ListBuffer[String]()
    var curPath = mutable.ListBuffer[String]()

    def makeCommand(buffer: ListBuffer[String]) = {
      val command = buffer.head
      val cd = """\$ cd (\w+)""".r

      val dir = """dir (\w+)""".r
      val file = """(\d+) (\w+\.?.*)""".r
      command match {
        case cd(dir) => {
          fileSystem.put(dir, _)
          curPath.addOne(dir)
        }
        case "$ cd /" => {
          curPath.clear()
          curPath.addOne("/")
        }
        case "$ cd .." => {
          if(curPath.size>1)
            curPath.remove(curPath.size-1)
        }
        case "$ ls" =>  {
          buffer.tail.foreach {
            case dir(dir) => curPath.foldLeft(fileSystem)((fs, path) => {
              fs(path).asInstanceOf[mutable.Map[String, AnyRef]]
            }).put(dir, mutable.Map[String, AnyRef]())
            case file(size, name) => {
              val curFold: mutable.Map[String, AnyRef] = curPath.foldLeft(fileSystem)((fs, path) => {
                fs(path).asInstanceOf[mutable.Map[String, AnyRef]]
              })

              curFold.put(name, size.toLong.asInstanceOf[AnyRef])
            }
          }
        }
      }
    }
    input.foreach(command => {
      if (command.startsWith("$")) {
        if (buffer.nonEmpty) {
          makeCommand(buffer)
        }
        buffer.clear()
      }
      buffer.addOne(command)
    })
    makeCommand(buffer)



    print(fileSystem)
    curPath.clear()
    var sumMap = mutable.Map[ListBuffer[String], Long]()
    val neighborsFunction: String => Iterable[String] = {
      folder => {
        curPath.addOne(folder)
        val curFold: mutable.Map[String, AnyRef] = curPath.foldLeft(fileSystem)((fs, path) => {
          fs(path).asInstanceOf[mutable.Map[String, AnyRef]]
        })
        val sum = curFold
          .filter(kv => kv._2.isInstanceOf[Long]).map(kv => kv._2.asInstanceOf[Long]).sum
        sumMap.put(curPath, sum)
        if(curFold.contains(folder) && curFold(folder).isInstanceOf[mutable.Map[String, AnyRef]]) {

          curFold(folder).asInstanceOf[mutable.Map[String, AnyRef]]
            .filter(e => e._2.isInstanceOf[mutable.Map[String, AnyRef]]).keys
        } else if(curFold.contains(folder)) {
          /*
          if (curPath.size > 1)
            curPath.remove(curPath.size - 1)
          curFold = curPath.foldLeft(fileSystem)((fs, path) => {
            fs(path).asInstanceOf[mutable.Map[String, AnyRef]]
          })

          curFold(folder).asInstanceOf[mutable.Map[String, AnyRef]]
            .filter(e => e._2.isInstanceOf[mutable.Map[String, AnyRef]]).keys

           */
          Iterable()
        } else {
          Iterable()
        }
      }

    }
    val value = Graphs.dfs("/")(neighborsFunction)


    println(sumMap)
    -1

  }


  def part2(input: Seq[String]): Int = {
    -1
  }
}
