package dev.gabrielsson

import dev.gabrielsson.GridExtensions.Grid
import dev.gabrielsson.Points.Point

import java.awt.{Color, Graphics2D}
import scala.collection.mutable
import scala.swing.event.{Key, KeyPressed}
import scala.swing.{Dimension, MainFrame, Panel, SimpleSwingApplication, Swing}

object Tetrix extends SimpleSwingApplication {
  val grid = mutable.HashMap[Point, Char]()
  val rand = new scala.util.Random
  val MAX_HEIGHT = 20
  val MAX_WIDTH = 7
  var keyStack = mutable.Stack[Char]()

  val init = for {
    x <- (0 until MAX_WIDTH)
    y <- (0 to MAX_HEIGHT)
  } yield Point(x, y)

  grid.addAll(init.map(p => (p, '.')).toMap)
  val listOfBlocks = List(
    Seq(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)),
    Seq(Point(1, 0), Point(1, -1), Point(1, -2), Point(2, -1), Point(0, -1)),
    Seq(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, -1), Point(2, -2)),
    Seq(Point(0, 0), Point(0, -1), Point(0, -2), Point(0, -3)),
    Seq(Point(0, 0), Point(0, -1), Point(1, 0), Point(1, -1))
  )

  var currBlock = listOfBlocks(1).moveBy(Point(2, 0))

  def onKeyPress(keyCode: Key.Value) = keyCode match {
    case Key.Left => keyStack.push('<')
    case Key.Right => keyStack.push('>')
    //case Key.Up => keyStack.push('^')
    case _ => println(s"Unknown ${keyCode}")
  }

  def onPaint(g: Graphics2D) {

    (grid ++ currBlock.map(p => (p, '@'))).foreach(kv => {
      kv._2 match {
        case '.' => g.setPaint(Color.GRAY)
        case '#' => g.setPaint(Color.RED)
        case '@' => g.setPaint(Color.GREEN)
      }

      g.fillRect(kv._1.x * 14, kv._1.y * 14, 14, 14)
    })
  }

  def top = new MainFrame {
    title = "Day 17 Gamification"
    val panel = mainPanel
    contents = panel
    val horizontalTimer = new javax.swing.Timer(10, Swing.ActionListener(e => {
      moveByKey()
      panel.repaint()
    }))
    val verticalTimer = new javax.swing.Timer(250, Swing.ActionListener(e => {
      moveDown()

      panel.repaint()
    }))
    horizontalTimer.start()
    verticalTimer.start()
  }

  def mainPanel = new Panel {
    preferredSize = new Dimension(700, 400)
    focusable = true
    listenTo(keys)

    reactions += {
      case KeyPressed(_, key, _, _) =>
        onKeyPress(key)
        repaint
    }

    override def paint(g: Graphics2D) {
      g fillRect(0, 0, size.width, size.height)
      onPaint(g)
    }
  }

  def moveByKey() = {
    keyStack.popAll().map(c => {
      val horizontal = c match {
        case '<' => Point(-1, 0)
        case '>' => Point(1, 0)
      }

      if (currBlock.canMove(horizontal, grid.toMap)) {
        currBlock = currBlock.moveBy(horizontal)
      }
    })
  }

  def moveDown() = {
    val down = Point(0, 1)

    if (currBlock.canMove(down, grid.toMap)) {
      println(s"Before ${currBlock}")
      currBlock = currBlock.moveBy(down)
      println(s"After ${currBlock}")
    } else {
      currBlock.foreach(p => grid(p) = '#')
      currBlock = listOfBlocks(rand.nextInt(4)).map(p => p + Point(2, 0))
    }
  }

  type Block = Seq[Point]

  implicit class BlockMove(val block: Block) {
    def moveBy(point: Point): Block = {
      block.map(p => {
        p + point
      })
    }

    def canMove(point: Point, grid: Grid[Char]): Boolean = {
      val newBlock = moveBy(point)
      val occupied: Boolean = {
        newBlock.map(p => grid.getOrElse(p, '.')).exists(c => c != '.')
      }
      !newBlock.exists(p => occupied || p.x < 0 || p.x > MAX_WIDTH - 1 || p.y > MAX_HEIGHT)
    }
  }

}
