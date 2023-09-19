package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */

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
  var currState = new GameState()
  var gameStateList : List[GameState] = List[GameState]()
  var reverse : Boolean = false

  initializeGame()

  def initializeGame(): (Unit) = {
    val startBody : List[Point] = List[Point] (Point(1,0), Point(0,0))
    val startHead : Point = Point(2,0)

    val currApple: Point = appleGenerator(startBody, startHead)
    currState = currState.copy(snakeBody = startBody, apple = currApple, snakeHead = startHead)

    gameStateList = gameStateList :+ currState
    directionFlag.right = true
  }

  def gameOver: Boolean = {
    currState.hit
  }

  def appleGenerator(snakeBody : List[Point], snakeHead : Point) : Point = {
    val freeList = gridDims.allPointsInside.filterNot(point => snakeBody.contains(point) || point == snakeHead)

    if (freeList.isEmpty){
      return null
    }

    val applePos : Int = random.randomInt(freeList.length)
    val applePlace : Point = freeList(applePos)
    applePlace
  }

  def moveSnake(direction: DirectionBools, snakeHead : Point): Point = {
    val (x,y) = (snakeHead.x, snakeHead.y)

    var newSnakeHead = snakeHead

    if(direction.right) {
      val newSnake : Point = Point(x + 1, y)
      newSnakeHead = newSnake
    }
    else if(direction.left) {
      val newSnake : Point = Point(x - 1, y)
      newSnakeHead = newSnake
    }
    else if(direction.up){
      val newSnake : Point = Point(x, y - 1)
      newSnakeHead = newSnake
    }
    else if(direction.down){
      val newSnake : Point = Point(x, y + 1)
      newSnakeHead = newSnake
    }
    newSnakeHead
  }

  def boundaryCheck(snakeHead : Point) : (Point, Boolean) = {
    var x : Int = snakeHead.x
    var y : Int = snakeHead.y

    var crossed : Boolean = false

    if(x >= gridDims.width){
      x = 0
      crossed = true
    }
    else if(y >= gridDims.height){
      y = 0
      crossed = true
    }
    else if(x < 0){
      x = gridDims.width
      crossed = true
    }
    else if(y < 0){
      y = gridDims.height
      crossed = true
    }
    val newSnakeHead : Point = Point(x,y)

    return (newSnakeHead, crossed)
  }

  def step(): Unit = {
    if(reverse){
      if(gameStateList.length == 1){
        currState = gameStateList.head
      }
      else{
        gameStateList = gameStateList.init
        currState = gameStateList.last
      }
      directionFlag.resetDirFlags()
      currState.snakeHeadDir match
      {
        case East() => directionFlag.right = true
        case West() => directionFlag.left = true
        case South() => directionFlag.down = true
        case North() => directionFlag.up = true
      }
      return
    }
    if (currState.hit){
      return
    } //stops the game moving

    val bodyWithHead : List[Point] = currState.snakeHead :: currState.snakeBody

    val snakeHeadMoved : Point = moveSnake(directionFlag, currState.snakeHead)
    currState = currState.copy(snakeHead = snakeHeadMoved)

    val snakeHeadOutOfBounds = boundaryCheck(currState.snakeHead)._1
    currState = currState.copy(snakeHead = snakeHeadOutOfBounds)

    if(boundaryCheck(currState.snakeHead)._2)
    {
      val snakeHeadOutOfBounds = moveSnake(directionFlag, currState.snakeHead)
      currState = currState.copy(snakeHead = snakeHeadOutOfBounds)
    }

    if(currState.counter == 0)
    {
      val newSnakeBody : List[Point] = bodyWithHead.init
      currState = currState.copy(snakeBody = newSnakeBody)
    }
    else
    {
      val newSnakeBody: List[Point] = bodyWithHead
      currState = currState.copy(snakeBody = newSnakeBody, counter = currState.counter - 1)
    }

    if(currState.snakeHead == currState.apple)
    {
      val currApple = appleGenerator(currState.snakeBody, currState.snakeHead)

      currState = currState.copy(apple = currApple, counter = currState.counter + 3)
    }

    if(currState.snakeBody.contains(currState.snakeHead))
    {
      currState = currState.copy(hit = true)
    }

    currState = currState.copy(forbiddenDir = currState.snakeHeadDir.opposite)
    gameStateList = gameStateList :+ currState
  }

  def changeDir(d: Direction): Unit = {
    if(currState.hit) {
      return
    } //doesn't allow for direction change after game over

    if(d != currState.forbiddenDir)
    {
      d match
      {
        case North() =>
          directionFlag.resetDirFlags()
          directionFlag.up = true
          currState = currState.copy(snakeHeadDir = North())

        case South() =>
          directionFlag.resetDirFlags()
          directionFlag.down = true
          currState = currState.copy(snakeHeadDir = South())

        case West() =>
          directionFlag.resetDirFlags()
          directionFlag.left = true
          currState = currState.copy(snakeHeadDir = West())

        case East() =>
          directionFlag.resetDirFlags()
          directionFlag.right = true
          currState = currState.copy(snakeHeadDir = East())
      }
    }
  }

  def getCellType(p : Point): CellType = {

    if(p == currState.snakeHead)
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

  def setReverse(r: Boolean): Unit = {
    reverse = r
  }
}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 2 // change this to increase/decrease speed of game

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
  Dimensions(width = 10, height = 10)  // you can adjust these values to play on a different sized board



}


