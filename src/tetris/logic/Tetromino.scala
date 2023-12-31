package tetris.logic

abstract class Tetromino() {
  def initialPositions(): List[Point]
  def rotateLeft(): List[Point]
  def rotateRight(): List[Point]
}

class ICellBlock(currentGameState: GameState) extends Tetromino {

  override def initialPositions (): List[Point] = {
    List(Point(0, 0), Point(-1, 0), Point(1, 0), Point(2, 0))
  }

  override def rotateLeft(): List[Point] = {
    val newShape = currentGameState.relativeBlockShape.map { point =>
      val newX = point.y
      val newY = -point.x + 1
      Point(newX, newY)
    }
    newShape
  }

  override def rotateRight(): List[Point] = {
    val newShape = currentGameState.relativeBlockShape.map { point =>
      val newX = -point.y + 1
      val newY = point.x
      Point(newX, newY)
    }
    newShape
  }

}

class OCellBlock(currentGameState: GameState) extends Tetromino {

  override def initialPositions(): List[Point] = {
    List(Point(0, 0), Point(0, -1), Point(1, -1), Point(1, 0))
  }

  override def rotateLeft(): List[Point] = {
    currentGameState.relativeBlockShape
  }

  override def rotateRight(): List[Point] = {
    currentGameState.relativeBlockShape
  }
}

class standardBlock (currentGameState: GameState, blockType: CellType) extends Tetromino {

  override def initialPositions (): List[Point] = {
    blockType match {
      case JCell => List(Point(0, 0), Point(-1, 0), Point(1, 0), Point(-1, -1))
      case LCell => List(Point(0, 0), Point(-1, -0), Point(1, 0), Point(1, -1))
      case SCell => List(Point(0, 0), Point(-1, 0), Point(0, -1), Point(1, -1))
      case TCell => List(Point(0, 0), Point(-1, 0), Point(1, 0), Point(0, -1))
      case ZCell => List(Point(0, 0), Point(0, -1), Point(-1, -1), Point(1, 0))
    }
  }

  override def rotateLeft(): List[Point] = {
    val newShape = currentGameState.relativeBlockShape.map { point =>
      val newX = point.y
      val newY = -point.x
      Point(newX, newY)
    }
    newShape
  }

  override def rotateRight(): List[Point] = {
    val newShape = currentGameState.relativeBlockShape.map { point =>
      val newX = -point.y
      val newY = point.x
      Point(newX, newY)
    }
    newShape
  }

}
