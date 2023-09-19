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
  var currState = new GameState()
  initializeGame(currState)

  var gameStateList : List[GameState] = List[GameState]()

  var reverse : Boolean = false

  def initializeGame(activeState : GameState): (Unit) = {

    val startBody : List[Point] = List[Point] (Point(1,0), Point(0,0))
    val newState = new GameState(snakeBody = startBody, snakeHead = Point(2,0))
    val currApple: Point = appleGenerator(newState.snakeBody, newState.snakeHead)
    val newState2 = new GameState(snakeBody = startBody, apple = currApple, snakeHead = Point(2,0))
    currState = newState2

    val newGameStateList = gameStateList :+ currState
    gameStateList = newGameStateList
    directionFlag.right = true
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

    val bodyWithHead : List[Point] = currState.snakeHead :: currState.snakeBody

    val snakeHead : Point = moveSnake(directionFlag, currState.snakeHead)
    val newGameState = new GameState(snakeBody = currState.snakeBody,
      snakeHead = snakeHead, apple = currState.apple, counter = currState.counter,
      hit = currState.hit, snakeHeadDir = currState.snakeHeadDir)
    var modifiedState = newGameState
    currState = modifiedState
    var newGameStateList = gameStateList :+ currState
    gameStateList = newGameStateList

    var movedSnake : Point = boundaryCheck(currState.snakeHead)._1
    modifiedState = new GameState(snakeBody = modifiedState.snakeBody, snakeHead = movedSnake,
      apple = modifiedState.apple, counter = modifiedState.counter, hit = modifiedState.hit,
      snakeHeadDir = modifiedState.snakeHeadDir)
    currState = modifiedState
    newGameStateList = gameStateList :+ currState
    gameStateList = newGameStateList

    if(reverse){
      gameStateList = backTrack(gameStateList)
    }

    if(boundaryCheck(currState.snakeHead)._2)
    {
      movedSnake = moveSnake(directionFlag, currState.snakeHead)
      modifiedState = new GameState(snakeBody = modifiedState.snakeBody, snakeHead = movedSnake,
        apple = modifiedState.apple, counter = modifiedState.counter, hit = modifiedState.hit,
        snakeHeadDir = modifiedState.snakeHeadDir)
      currState = modifiedState
      newGameStateList = gameStateList :+ currState
      gameStateList = newGameStateList
    }

    if(currState.counter == 0)
    {
      val newSnakeBody : List[Point] = bodyWithHead.init
      modifiedState = new GameState(snakeBody =  newSnakeBody, snakeHead = currState.snakeHead,
        apple = currState.apple, counter = currState.counter, hit = currState.hit,
        snakeHeadDir = currState.snakeHeadDir)
      currState = modifiedState
      newGameStateList = gameStateList :+ currState
      gameStateList = newGameStateList
    }
    else
    {
      val newSnakeBody: List[Point] = bodyWithHead
      modifiedState = new GameState(snakeBody = newSnakeBody, snakeHead = currState.snakeHead,
        apple = currState.apple, counter = currState.counter - 1, hit = currState.hit,
        snakeHeadDir = currState.snakeHeadDir)
      currState = modifiedState
      newGameStateList = gameStateList :+ currState
      gameStateList = newGameStateList
    }

    if(currState.snakeHead == currState.apple)
    {
      val currApple = appleGenerator(currState.snakeBody, currState.snakeHead)
      modifiedState = new GameState(snakeBody = currState.snakeBody, snakeHead = currState.snakeHead,
        apple = currApple, counter = currState.counter + 3, hit = currState.hit,
        snakeHeadDir = currState.snakeHeadDir)
      currState = modifiedState
    }

    if(currState.snakeBody.contains(currState.snakeHead))
    {
      modifiedState = new GameState(snakeBody = currState.snakeBody, snakeHead = currState.snakeHead,
        apple = currState.apple, counter = currState.counter, hit = true,
        snakeHeadDir = currState.snakeHeadDir)
      currState = modifiedState
    }
    modifiedState = new GameState(snakeBody = currState.snakeBody, snakeHead = currState.snakeHead,
      apple = currState.apple, counter = currState.counter, hit = currState.hit,
      snakeHeadDir = currState.snakeHeadDir, forbiddenDir = currState.snakeHeadDir.opposite)
    currState = modifiedState
  }

  def changeDir(d: Direction): Unit = {
    if(currState.hit) {
      return
    } //doesn't allow for direction change after game over
    var newGameState : GameState = new GameState()

    if(d != currState.forbiddenDir)
    {
      d match
      {
        case North() =>
          directionFlag.resetDirFlags()
          directionFlag.up = true
          newGameState = new GameState(snakeBody = currState.snakeBody,
            snakeHead = currState.snakeHead, apple = currState.apple, counter = currState.counter,
            hit = currState.hit, snakeHeadDir = North())
          currState = newGameState

        case South() =>
          directionFlag.resetDirFlags()
          directionFlag.down = true
          newGameState = new GameState(snakeBody = currState.snakeBody,
            snakeHead = currState.snakeHead, apple = currState.apple, counter = currState.counter,
            hit = currState.hit, snakeHeadDir = South())
          currState = newGameState

        case West() =>
          directionFlag.resetDirFlags()
          directionFlag.left = true
          newGameState = new GameState(snakeBody = currState.snakeBody,
            snakeHead = currState.snakeHead, apple = currState.apple, counter = currState.counter,
            hit = currState.hit, snakeHeadDir = West())
          currState = newGameState

        case East() =>
          directionFlag.resetDirFlags()
          directionFlag.right = true
          newGameState = new GameState(snakeBody = currState.snakeBody,
            snakeHead = currState.snakeHead, apple = currState.apple, counter = currState.counter,
            hit = currState.hit, snakeHeadDir = East())
          currState = newGameState
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

  def backTrack(gameStateList : List[GameState]) : List[GameState] = {
    var gameStateListCopy = gameStateList
    while(reverse){
      val newGameStateList = gameStateList.init
      gameStateListCopy = newGameStateList
    }
    return gameStateListCopy
  }

  def setReverse(r: Boolean): Unit = {
    reverse = r
  }
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


