package snake.logic

case class GameState(val snakeBody: List[Point] = List[Point](), val snakeHead : Point = Point(2,0),
                val apple : Point = Point(0,0), val counter : Int = 0, val hit : Boolean = false,
                val snakeHeadDir : Direction = East(), val forbiddenDir : Direction = West()){
}

