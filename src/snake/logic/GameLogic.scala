package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._

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

  //make a var that is the first index to start the list, and whenever a new location is added, just add a new element to the list. Then, when reverse you just move that index back as many steps needed

  var snakeHeadPos: Point = Point(2, 0)
  var snakeHeadDir: Direction = East()
  var snakeBodyPos: List[Point] = List[Point](Point(1,0), Point(0,0))

  def gameOver: Boolean = {
    tailHit
  }

  def appleGenerator() : Point = {
    var free_list = gridDims.allPointsInside.filterNot(point=> snakeBodyPos.contains(point) || point == snakeHeadPos)

    if (free_list.isEmpty){
      return null
    }

    val applePos : Int = random.randomInt(free_list.length)
    val applePlace : Point = free_list(applePos)
    applePlace
  }

  var tailHit : Boolean = false

  var counter : Int = 0

  var currApple: Point = appleGenerator()

  var (x, y) = initializeGame()
  snakeHeadPos = Point(x, y)
  snakeHeadDir = East()
  var directionChanged : Boolean = false

  def step(): Unit = {
    if (tailHit){
      return
    } //stops the game moving

    directionChanged = false
    val bodyWithHeadSnake : List[Point] = snakeHeadPos :: snakeBodyPos

    if(directionFlag.down) //TODO make it a function?
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

    //Updates the snake body positions
    if(counter == 0)
    {
      val tempSnakeCopy : List[Point] = bodyWithHeadSnake.init
      val newSnakeBodyPos : List[Point] = tempSnakeCopy
      snakeBodyPos = newSnakeBodyPos
    }
    else
    {
      val newSnakeBodyPos: List[Point] = bodyWithHeadSnake
      snakeBodyPos = newSnakeBodyPos
      counter -= 1
    }

    if(snakeHeadPos == currApple) //TODO make it a function
    {
      currApple = appleGenerator()
      counter += 3
    }

    if(x >= gridDims.width) //TODO make it a function?
      {
        x = 0
        snakeHeadPos = Point(x,y)
        val tempSnakeCopy: List[Point] = bodyWithHeadSnake.init
        val newSnakeBodyPos: List[Point] = tempSnakeCopy
        snakeBodyPos = newSnakeBodyPos
      }
    if(y >= gridDims.height)
      {
        y = 0
        snakeHeadPos = Point(x, y)
        val tempSnakeCopy: List[Point] = bodyWithHeadSnake.init
        val newSnakeBodyPos: List[Point] = tempSnakeCopy
        snakeBodyPos = newSnakeBodyPos
      }
    if(x < 0)
      {
        x = gridDims.width
        snakeHeadPos = Point(x, y)
        val tempSnakeCopy: List[Point] = bodyWithHeadSnake.init
        val newSnakeBodyPos: List[Point] = tempSnakeCopy
        snakeBodyPos = newSnakeBodyPos
      }
    if(y < 0)
      {
        y = gridDims.height
        snakeHeadPos = Point(x, y)
        val tempSnakeCopy: List[Point] = bodyWithHeadSnake.init
        val newSnakeBodyPos: List[Point] = tempSnakeCopy
        snakeBodyPos = newSnakeBodyPos
      }

    if(snakeBodyPos.contains(snakeHeadPos))
      tailHit = true
  }

  def changeDir(d: Direction): Unit = {
    snakeHeadDir = d

  if(tailHit) {
    return
  } //doesn't allow for direction change after game over

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

  def getCellType(p : Point): CellType = {

    if(p == snakeHeadPos)
      {
        return SnakeHead(snakeHeadDir)
      }
    else if(snakeBodyPos.contains(p))
      {
        val numOfBodyBlocks : Int = snakeBodyPos.length - 1
        val colorStep : Float = 1 / numOfBodyBlocks

        return SnakeBody(colorStep * snakeBodyPos.indexOf(p))
      }
    else if(p == currApple)
      {
        return Apple()
      }
    else
      return Empty()
  }

  def setReverse(r: Boolean): Unit = ()

}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 1 // change this to increase/decrease speed of game

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


