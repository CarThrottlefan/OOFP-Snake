package snake.logic

class GameState(val snakeBody: List[Point] = List[Point](), val snakeHead : Point = Point(2,0),
                val apple : Point = Point(0,0), val counter : Int = 0, val hit : Boolean = false,
                val snakeHeadDir : Direction = East()){
  //val x: Int = 2
  //val y: Int = 0

  //val snakeBody: List[Point] = List[Point] ()
  //snakeHead = snakeHeadMaker(x, y)

  /*def snakeHeadMaker(x : Int, y : Int) : Point = {
    val head : Point = Point(x, y)
    head
  }*/


  def ::(currBody : List[Point], newPoint : Point) : List[Point] = {
    val newBody : List[Point] = newPoint :: currBody
    return newBody
  }

  //val apple: Point = Point(0, 0)
  //val counter : Int = 0
  //val hit : Boolean = false
  //val snakeHeadDir: Direction = East()
}

