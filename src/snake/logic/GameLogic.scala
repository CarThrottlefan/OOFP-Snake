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
  def initializeGame(): (GameState) = {

    val activeGame = new GameState()
    val startBody : List[Point] = List[Point] (Point(1,0), Point(0,0))
    activeGame.snakeBody = startBody

    val currApple: Point = appleGenerator(activeGame.snakeBody, activeGame.snakeHead(activeGame.x, activeGame.y))

    directionFlag.right = true
    activeGame.apple = currApple

    return activeGame
  }

  //make a var that is the first index to start the list, and whenever a new location is added, just add a new element to the list. Then, when reverse you just move that index back as many steps needed

  //var snakeHeadPos: Point = Point(2, 0)
 // var snakeHeadDir: Direction = East()
  //var snakeBodyPos: List[Point] = List[Point](Point(1,0), Point(0,0))

  def gameOver: Boolean = {
    currState.hit
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

  //var tailHit : Boolean = false

  //var counter : Int = 0

  //var currApple: Point = appleGenerator(snakeBodyPos, snakeHeadPos)

  //var (x, y) = initializeGame()
  //snakeHeadPos = Point(x, y)
  //var snakeHeadDir: Direction = East()
  //var directionChanged : Boolean = false

  //val currState = new GameState()
  val currState: GameState = initializeGame()

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

    if (currState.hit){
      return
    } //stops the game moving

    //directionChanged = false


    //var snakeHead = currState.snakeHead(currState.x,currState.y)
    //var snakeBody = currState.snakeBody
    val bodyWithHead : List[Point] = currState.currSnakeHead :: currState.snakeBody

    currState.currSnakeHead = moveSnake(directionFlag, currState.currSnakeHead)
    //var (x,y) = (snakeHead.x, snakeHead.y)

    //Updates the snake body positions
    if(currState.counter == 0)
    {
      val newSnakeBodyPos : List[Point] = bodyWithHead.init
      currState.snakeBody = newSnakeBodyPos
    }
    else
    {
      val newSnakeBodyPos: List[Point] = bodyWithHead
      currState.snakeBody = newSnakeBodyPos
      currState.counter -= 1
    }

    if(currState.currSnakeHead == currState.apple) //TODO make it a function
    {
      currState.apple = appleGenerator(currState.snakeBody, currState.currSnakeHead)
      currState.counter += 3
    }

    if(currState.x >= gridDims.width) //TODO make it a function?
    {
      currState.x = 0
      currState.currSnakeHead = currState.snakeHead(currState.x, currState.y)
      //val tempSnakeCopy: List[Point] = bodyWithHead.init
      //val newSnakeBodyPos: List[Point] = tempSnakeCopy
      //snakeBody = newSnakeBodyPos
    }
    if(currState.y >= gridDims.height)
    {
      currState.y = 0
      currState.currSnakeHead = currState.snakeHead(currState.x, currState.y)
      //val tempSnakeCopy: List[Point] = bodyWithHead.init
      //val newSnakeBodyPos: List[Point] = tempSnakeCopy
      //snakeBody = newSnakeBodyPos
    }
    if(currState.x < 0)
    {
      currState.x = gridDims.width
      currState.currSnakeHead = currState.snakeHead(currState.x, currState.y)
      //val tempSnakeCopy: List[Point] = bodyWithHead.init
      //val newSnakeBodyPos: List[Point] = tempSnakeCopy
      //snakeBody = newSnakeBodyPos
    }
    if(currState.y < 0)
    {
      currState.y = gridDims.height
      currState.currSnakeHead = currState.snakeHead(currState.x, currState.y)
      //val tempSnakeCopy: List[Point] = bodyWithHead.init
      //val newSnakeBodyPos: List[Point] = tempSnakeCopy
      //snakeBody = newSnakeBodyPos
    }

    if(currState.snakeBody.contains(currState.currSnakeHead))
      currState.hit = true
  }

  def changeDir(d: Direction): Unit = {
    currState.snakeHeadDir = d

    if(currState.hit) {
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

    if(p == currState.currSnakeHead)
    {
      return SnakeHead(currState.snakeHeadDir)
    }
    else if(currState.snakeBody.contains(p))
    {
      val numOfBodyBlocks : Int = currState.snakeBody.length - 1
      val colorStep : Float = 1 / numOfBodyBlocks

      return SnakeBody(colorStep * currState.snakeBody.indexOf(p))
    }
    else if(p == currState.apple)
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
  Dimensions(width = 4, height = 2)  // you can adjust these values to play on a different sized board



}


