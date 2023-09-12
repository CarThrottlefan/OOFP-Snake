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

  var up: Boolean = false

 /* def directionBools() = {
    var up: Boolean = false
    var down: Boolean = false
    var left: Boolean = false
    var right: Boolean = false
}*/

  case class DirectionBools(var up: Boolean = false, var down: Boolean = false, var left: Boolean = false, var right: Boolean = false)
  {
    sealed trait Direction
    case class North() extends Direction
    case class South() extends Direction
    case class West() extends Direction
    case class East() extends Direction
  }


  def gameOver: Boolean = false

  // TODO implement me
  def step(): Unit = ()

  // TODO implement me
  var directionFlag: DirectionBools = DirectionBools()
  def changeDir(d: Direction): Unit = {
    //test = true

    d match
    {
      case North() =>
        directionFlag.up = true

      case South() =>
        directionFlag.down = true

      case West() =>
        directionFlag.left = true

      case East() =>
        directionFlag.right = true
    }


  // TODO implement me
  def getCellType(p : Point): CellType = {
    if(directionFlag.up)
      {return SnakeHead(North())}
    else if(directionFlag.left)
      return SnakeHead(West())
    else
      return SnakeHead(South())
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


