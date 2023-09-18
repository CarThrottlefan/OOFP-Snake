package snake.logic

class GameState(){
  var x: Int = 2
  var y: Int = 0

  var snakeBody: List[Point] = List[Point] ()

  var currSnakeHead : Point = snakeHead(x, y)

  def snakeHead(x : Int, y : Int) : Point = {
    val head : Point = Point(x, y)
    head
  }

  /*def snakeBody(currBody : List[Point]) : List[Point] = {
    return currBody
  }*/

  def ::(currBody : List[Point], newPoint : Point) : List[Point] = {
    val newBody : List[Point] = newPoint :: currBody
    return newBody
  }

  //var snakeHead : Point = Point(x, y)

  var apple: Point = Point(0, 0)

  var counter : Int = 0

  var hit : Boolean = false

  var snakeHeadDir: Direction = East()
}