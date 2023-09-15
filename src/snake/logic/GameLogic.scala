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


  //var currApple : Point = Point (3,3)

  var snakeBodyPos: List[Point] = List[Point](Point(1,0), Point(0,0))
  //val snakeBodyPos : ListBuffer[Point] = ListBuffer (snakeHeadPos, Point(1,0), Point(0,0))

  def gameOver: Boolean = {
    tailHit
  }

  def appleGenerator() : Point = {
    //val freeSpotsNum : Int = gridDims.width * gridDims.height - snakeBodyPos.lengt

    var free_list = gridDims.allPointsInside.filterNot(point=> snakeBodyPos.contains(point) || point == snakeHeadPos)

    if (free_list.isEmpty){
      return null
    }

    val applePos : Int = random.randomInt(free_list.length)
    val applePlace : Point = free_list(applePos)
    applePlace
  }

  // TODO implement me

  var tailHit : Boolean = false

  var counter : Int = 0

  var currApple: Point = appleGenerator()
  //var appleExists : Boolean = true

  var (x, y) = initializeGame()
  snakeHeadPos = Point(x, y)
  snakeHeadDir = East()

  def step(): Unit = {

    if (tailHit) return //stops the game moving

    val bodyWithHeadSnake : List[Point] = snakeHeadPos :: snakeBodyPos

    var listLength : Int = snakeBodyPos.length - 1

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

    // Updates the snake body positions
    /*for (i <- (1 to listLength).reverse) {
      var newSnakeBody : List[Point] = List[Point]()
      snakeBodyPos(i) = snakeBodyPos(i - 1)*
    }*/

    //Updates the snake body positions
    if(counter == 0)
    {
      val tempSnakeCopy : List[Point] = bodyWithHeadSnake.init
      val newSnakeBodyPos : List[Point] = tempSnakeCopy
      //val newSnakeBodyPos : List[Point] = snakeHeadPos :: tempSnakeCopy
      snakeBodyPos = newSnakeBodyPos
    }
    else
    {
      val newSnakeBodyPos: List[Point] = bodyWithHeadSnake
      //val newSnakeBodyPos : List[Point] = snakeHeadPos :: tempSnakeCopy
      snakeBodyPos = newSnakeBodyPos
      counter -= 1
    }

   /* if (!(appleExists)) {
      currApple = appleGenerator()
      appleExists = true
    }*/


    // Updates the first element of the snake body to follow the head
   // snakeBodyPos(0) = snakeHeadPos

    if(snakeHeadPos == currApple) //TODO make it a function
    {
      //appleExists = false

      currApple = appleGenerator()

      val tailX : Int = snakeBodyPos(listLength).x
      val tailY : Int = snakeBodyPos(listLength).y

      /*if(directionFlag.up)
      {
        /*snakeBodyPos.append(Point(tailX, tailY + 1))
        snakeBodyPos.append(Point(tailX, tailY + 2))
        snakeBodyPos.append(Point(tailX, tailY + 3))*/
        //TODO maybe a function to add stuff to a list?
        val newElements : List[Point] = List[Point] (Point(tailX, tailY + 1), Point(tailX, tailY + 2), Point(tailX, tailY + 3))
        val newSnakeBodyPos : List[Point] = snakeBodyPos ::: newElements
        snakeBodyPos = newSnakeBodyPos
      }
      else if(directionFlag.down)
      {
        /*snakeBodyPos.append(Point(tailX, tailY - 1))
        snakeBodyPos.append(Point(tailX, tailY - 2))
        snakeBodyPos.append(Point(tailX, tailY - 3))*/

        val newElements: List[Point] = List[Point](Point(tailX, tailY - 1), Point(tailX, tailY - 2), Point(tailX, tailY - 3))
        val newSnakeBodyPos: List[Point] = snakeBodyPos ::: newElements
        snakeBodyPos = newSnakeBodyPos
      }
      else if(directionFlag.right)
      {
        /*snakeBodyPos.append(Point(tailX - 1, tailY))
        snakeBodyPos.append(Point(tailX - 2, tailY))
        snakeBodyPos.append(Point(tailX - 3, tailY))*/

        val newElements: List[Point] = List[Point](Point(tailX - 1, tailY), Point(tailX - 2, tailY), Point(tailX - 3, tailY))
        val newSnakeBodyPos: List[Point] = snakeBodyPos ::: newElements
        snakeBodyPos = newSnakeBodyPos
      }
      else
      {
        /*snakeBodyPos.append(Point(tailX + 1, tailY))
        snakeBodyPos.append(Point(tailX + 2, tailY))
        snakeBodyPos.append(Point(tailX + 3, tailY)) */
        val newElements: List[Point] = List[Point](Point(tailX + 1, tailY), Point(tailX + 2, tailY), Point(tailX + 3, tailY))
        val newSnakeBodyPos: List[Point] = snakeBodyPos ::: newElements
        snakeBodyPos = newSnakeBodyPos
      }*/
      counter = 3
      listLength += 3
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
    //test = true
    snakeHeadDir = d

    if(tailHit) return

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


