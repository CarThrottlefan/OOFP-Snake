package snake.logic

class GameState(val snakeBody: List[Point] = List[Point](), val snakeHead : Point = Point(2,0),
                val apple : Point = Point(0,0), val counter : Int = 0, val hit : Boolean = false,
                val snakeHeadDir : Direction = East(), val forbiddenDir : Direction = West()){

  /*def ::(currBody : List[Point], newPoint : Point) : List[Point] = {
    val newBody : List[Point] = newPoint :: currBody
    return newBody
  }*/
}

