package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._
import scala.collection.mutable.ListBuffer // TODO maybe a way without it?

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */

//For reverse: When you play normally, save curr game save on stack then when press reverse just pop from stack


class GameLogic(val random: RandomGenerator,
                val gridDims : Dimensions) {
  case class DirectionBools(var up: Boolean = false, var down: Boolean = false, var left: Boolean = false, var right: Boolean = false)
  {
    sealed trait Direction
    case class North() extends Direction
    case class South() extends Direction
    case class West() extends Direction
    case class East() extends Direction

    def resetDirFlags() : Unit = {
      up = false
      down = false
      left = false
      right = false
    }
  }

  var directionFlag: DirectionBools = DirectionBools()
  def initializeGame(): (Int, Int) = {
    val x : Int = 2
    val y : Int = 0

    directionFlag.right = true
    (x,y)
  }

  var (x,y) = initializeGame()

  //make a var that is the first index to start the list, and whenever a new location is added, just add a new element to the list. Then, when reverse you just move that index back as many steps needed


  var snakeHeadPos : Point = Point(x, y)
  var snakeHeadDir : Direction = East()

  val snakeBodyPos : ListBuffer[Point] = ListBuffer (snakeHeadPos, Point(1,0), Point(0,0))

  def gameOver: Boolean = false

  // TODO implement me


  def step(): Unit = {

    if(directionFlag.down)
    {
      y += 1
      snakeHeadPos = Point(x,y)
    }
    else if (directionFlag.up)
    {
      y -= 1
      snakeHeadPos = Point(x, y)
    }
    else if (directionFlag.left)
    {
      x -= 1
      snakeHeadPos = Point(x, y)
    }
    else if (directionFlag.right)
    {
      x += 1
      snakeHeadPos = Point(x, y)
    }

    // Updates the snake body positions
    for (i <- (1 until snakeBodyPos.length).reverse) {
      snakeBodyPos(i) = snakeBodyPos(i - 1)
    }

    // Updates the first element of the snake body to follow the head
    snakeBodyPos(0) = snakeHeadPos

    if(x >= gridDims.width)
      x = 0
    if(y >= gridDims.height)
      y = 0
    if(x < 0)
      x = gridDims.width
    if(y < 0)
      y = gridDims.height
  }

  def changeDir(d: Direction): Unit = {
    //test = true
    snakeHeadDir = d

    d match
    {
      case North() =>
        directionFlag.resetDirFlags()
        directionFlag.up = true

      case South() =>
        directionFlag.resetDirFlags()
        directionFlag.down = true

      case West() =>
        directionFlag.resetDirFlags()
        directionFlag.left = true

      case East() =>
        directionFlag.resetDirFlags()
        directionFlag.right = true
    }
  }

  // TODO implement me
  def getCellType(p : Point): CellType = {

    if(p == snakeHeadPos)
      {
        return SnakeHead(snakeHeadDir)
      }
    else if(snakeBodyPos.contains(p))
      {
        return SnakeBody()
      }

    /*if(directionFlag.up) {
      //directionFlag.resetDirFlags()
      return SnakeHead(North())
    }
    else if(directionFlag.left) {
      //directionFlag.resetDirFlags()
      return SnakeHead(West())
    }
    else if(directionFlag.right) {
      //directionFlag.resetDirFlags()
      return SnakeHead(East())
    }
    //directionFlag.resetDirFlags()
    else if(directionFlag.up) {
      return SnakeHead(South())
    }*/
    else
      return Empty()
  }



  // TODO implement me
  def setReverse(r: Boolean): Unit = ()

}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller

  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultGridDims.width and DefaultGridDims.height
  val DefaultGridDims
    : Dimensions =
    Dimensions(width = 25, height = 25)  // you can adjust these values to play on a different sized board



}


