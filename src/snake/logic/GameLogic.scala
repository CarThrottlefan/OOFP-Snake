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
  val currState = new GameState()
  initializeGame(currState)

  def initializeGame(activeGame : GameState): (Unit) = {

    val startBody : List[Point] = List[Point] (Point(1,0), Point(0,0))
    activeGame.snakeBody = startBody

    val currApple: Point = appleGenerator(activeGame.snakeBody, activeGame.snakeHead(activeGame.x, activeGame.y))

    directionFlag.right = true
    activeGame.apple = currApple

  }

  //make a var that is the first index to start the list, and whenever a new location is added, just add a new element to the list. Then, when reverse you just move that index back as many steps needed

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

    if (currState.hit){
      return
    } //stops the game moving

    val bodyWithHead : List[Point] = currState.currSnakeHead :: currState.snakeBody

    currState.currSnakeHead = moveSnake(directionFlag, currState.currSnakeHead)
    currState.currSnakeHead = boundaryCheck(currState.currSnakeHead)._1

    if(boundaryCheck(currState.currSnakeHead)._2)
    {
      currState.currSnakeHead = moveSnake(directionFlag, currState.currSnakeHead)
    }

    currState.x = currState.currSnakeHead.x
    currState.y = currState.currSnakeHead.y

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

    if(currState.currSnakeHead == currState.apple)
    {
      currState.apple = appleGenerator(currState.snakeBody, currState.currSnakeHead)
      currState.counter += 3
    }

    //currState.x = currState.currSnakeHead.x
    //currState.y = currState.currSnakeHead.y

    if(currState.snakeBody.contains(currState.currSnakeHead))
      currState.hit = true
  }

  def changeDir(d: Direction): Unit = {

    if(currState.hit) {
      return
    } //doesn't allow for direction change after game over

    if(d.opposite != currState.snakeHeadDir)
    {
      d match
      {
        case North() =>
          directionFlag.resetDirFlags()
          directionFlag.up = true
          currState.snakeHeadDir = North()

        case South() =>
          directionFlag.resetDirFlags()
          directionFlag.down = true
          currState.snakeHeadDir = South()

        case West() =>
          directionFlag.resetDirFlags()
          directionFlag.left = true
          currState.snakeHeadDir = West()

        case East() =>
          directionFlag.resetDirFlags()
          directionFlag.right = true
          currState.snakeHeadDir = East()
      }
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


