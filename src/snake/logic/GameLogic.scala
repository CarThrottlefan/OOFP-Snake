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
  def initializeGame(): (Point, List[Point], Point) = {
    val x : Int = 2
    val y : Int = 0

    val snakeBody : List[Point] = List[Point](Point(1,0), Point(0,0))

    val snakeHead = Point (x, y)

    val currApple: Point = appleGenerator(snakeBody, snakeHead)

    directionFlag.right = true
    var activeGame = new GameState(snakeHead, snakeBody, currApple)
    (snakeHead, snakeBody, currApple)
  }

  //make a var that is the first index to start the list, and whenever a new location is added, just add a new element to the list. Then, when reverse you just move that index back as many steps needed

  //var snakeHeadPos: Point = Point(2, 0)
 // var snakeHeadDir: Direction = East()
  //var snakeBodyPos: List[Point] = List[Point](Point(1,0), Point(0,0))

  def gameOver: Boolean = {
    tailHit
  }

  def appleGenerator(snakeBody : List[Point], snakeHead : Point) : Point = {
    val free_list = gridDims.allPointsInside.filterNot(point => snakeBody.contains(point) || point == snakeHead)

    if (free_list.isEmpty){
      return null
    }

    val applePos : Int = random.randomInt(free_list.length)
    val applePlace : Point = free_list(applePos)
    applePlace
  }

  var tailHit : Boolean = false

  var counter : Int = 0

  //var currApple: Point = appleGenerator(snakeBodyPos, snakeHeadPos)

  //var (x, y) = initializeGame()
  //snakeHeadPos = Point(x, y)
  var snakeHeadDir: Direction = East()
  //var directionChanged : Boolean = false

  var (snakeHeadPos, snakeBodyPos, currApple) = initializeGame()

  def moveSnake(direction: DirectionBools, snakeHead : Point): Point = {
    val (x,y) = (snakeHead.x, snakeHead.y)

    var snake = snakeHead

    if(direction.right) {
      val newSnake : Point = Point(x + 1, y)
      snake = newSnake
    }
    else if(direction.left) {
      val newSnake : Point = Point(x - 1, y)
      snake = newSnake
    }
    else if(direction.up){
      val newSnake : Point = Point(x, y - 1)
      snake = newSnake
    }
    else if(direction.down){
      val newSnake : Point = Point(x, y + 1)
      snake = newSnake
    }
    snake
  }

  /*def getCounter(count : Int) : Unit = {

  }*/

  def step(): Unit = {
    if (tailHit){
      return
    } //stops the game moving

    //directionChanged = false
    val bodyWithHeadSnake : List[Point] = snakeHeadPos :: snakeBodyPos

    snakeHeadPos = moveSnake(directionFlag, snakeHeadPos)
    var (x,y) = (snakeHeadPos.x, snakeHeadPos.y)

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
      currApple = appleGenerator(snakeBodyPos, snakeHeadPos)
      counter += 3
    }

    if(x >= gridDims.width) //TODO make it a function?
    {
      x = 0
      snakeHeadPos = Point(x,y)
      //val tempSnakeCopy: List[Point] = bodyWithHeadSnake.init
      //val newSnakeBodyPos: List[Point] = tempSnakeCopy
      //snakeBodyPos = newSnakeBodyPos
    }
    if(y >= gridDims.height)
    {
      y = 0
      snakeHeadPos = Point(x, y)
      //val tempSnakeCopy: List[Point] = bodyWithHeadSnake.init
      //val newSnakeBodyPos: List[Point] = tempSnakeCopy
      //snakeBodyPos = newSnakeBodyPos
    }
    if(x < 0)
    {
      x = gridDims.width
      snakeHeadPos = Point(x, y)
      //val tempSnakeCopy: List[Point] = bodyWithHeadSnake.init
      //val newSnakeBodyPos: List[Point] = tempSnakeCopy
      //snakeBodyPos = newSnakeBodyPos
    }
    if(y < 0)
    {
      y = gridDims.height
      snakeHeadPos = Point(x, y)
      //val tempSnakeCopy: List[Point] = bodyWithHeadSnake.init
      //val newSnakeBodyPos: List[Point] = tempSnakeCopy
      //snakeBodyPos = newSnakeBodyPos
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


