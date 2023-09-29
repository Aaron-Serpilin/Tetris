package tetris.logic

abstract class Tetromino() {
  def initialPositions(): List[Point]
  def rotateLeft(): List[Point]
  def rotateRight(): List[Point]
}

class ICellBlock(currentGameState: GameState) extends Tetromino {

  override def initialPositions (): List[Point] = {
    List(Point(currentGameState.anchorPosition.x - 2, 1), Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x, 1), Point(currentGameState.anchorPosition.x + 1, 1))
  }

  override def rotateLeft(): List[Point] = {
    val newShape = currentGameState.currentBlockShape.map { point =>
      val newX = point.y + currentGameState.anchorPosition.x
      val newY = -point.x + 1 + currentGameState.anchorPosition.x
      Point(newX, newY)
    }
    newShape
  }

  override def rotateRight(): List[Point] = {
    val newShape = currentGameState.currentBlockShape.map { point =>
      val newX = -point.y + 1 + currentGameState.anchorPosition.x
      val newY = point.x - currentGameState.anchorPosition.x
      Point(newX, newY)
    }
    newShape
  }

}

class OCellBlock(currentGameState: GameState) extends Tetromino {

  override def initialPositions(): List[Point] = {
    List(Point(currentGameState.anchorPosition.x - 1, 0), Point(currentGameState.anchorPosition.x, 0), Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x, 1))
  }

  override def rotateLeft(): List[Point] = {
    currentGameState.currentBlockShape
  }

  override def rotateRight(): List[Point] = {
    currentGameState.currentBlockShape
  }
}

class standardBlock (currentGameState: GameState, blockType: CellType) extends Tetromino {

  override def initialPositions (): List[Point] = {
    blockType match {
      case JCell => List(Point(currentGameState.anchorPosition.x - 2, 0), Point(currentGameState.anchorPosition.x - 2, 1), Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x, 1))
      case LCell => List(Point(currentGameState.anchorPosition.x - 2, 1), Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x, 1), Point(currentGameState.anchorPosition.x, 0))
      case SCell => List(Point(currentGameState.anchorPosition.x - 2, 1), Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x - 1, 0), Point(currentGameState.anchorPosition.x, 0))
      case TCell => List(Point(currentGameState.anchorPosition.x - 2, 1), Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x, 1), Point(currentGameState.anchorPosition.x - 1, 0))
      case ZCell => List(Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x, 1), Point(currentGameState.anchorPosition.x - 1, 0), Point(currentGameState.anchorPosition.x - 2, 0))
    }
  }

  override def rotateLeft(): List[Point] = {
    val newShape = currentGameState.currentBlockShape.map { point =>
      val newX = point.y
      val newY = -point.x
      Point(newX, newY)
    }
    newShape
  }

  override def rotateRight(): List[Point] = {
    val newShape = currentGameState.currentBlockShape.map { point =>
      val newX = -point.y
      val newY = point.x
      Point(newX, newY)
    }
    newShape
  }

}
