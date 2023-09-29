package tetris.logic

abstract class Tetromino() {
  def rotateLeft(): Unit = {}
  def rotateRight(): Unit = {}
}

class ICellBlock(anchorPoint: Int) extends Tetromino {

  def initialPositions (currentGameState: GameState): List[Point] = {
    List(Point(currentGameState.anchorPoint - 2, 1), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1), Point(currentGameState.anchorPoint + 1, 1))
  }

  override def rotateLeft(): Unit = {

  }

  override def rotateRight(): Unit = {

  }

}

class OCellBlock(anchorPoint: Int) extends Tetromino {

  def initialPositions(currentGameState: GameState): List[Point] = {
    List(Point(currentGameState.anchorPoint - 1, 0), Point(currentGameState.anchorPoint, 0), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1))
  }

  override def rotateLeft(): Unit = {

  }

  override def rotateRight(): Unit = {

  }
}

class standardBlock (anchorPoint: Int) extends Tetromino {

  def initialPositions (blockType: CellType, currentGameState: GameState): List[Point] = {
    blockType match {
      case JCell => List(Point(currentGameState.anchorPoint - 2, 0), Point(currentGameState.anchorPoint - 2, 1), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1))
      case LCell => List(Point(currentGameState.anchorPoint - 2, 1), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1), Point(currentGameState.anchorPoint, 0))
      case SCell => List(Point(currentGameState.anchorPoint - 2, 1), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint - 1, 0), Point(currentGameState.anchorPoint, 0))
      case TCell => List(Point(currentGameState.anchorPoint - 2, 1), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1), Point(currentGameState.anchorPoint - 1, 0))
      case ZCell => List(Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1), Point(currentGameState.anchorPoint - 1, 0), Point(currentGameState.anchorPoint - 2, 0))
    }
  }

  override def rotateLeft(): Unit = {

  }

  override def rotateRight(): Unit = {

  }

}


//class centeredTetrominos extends Tetromino {
//  override def rotateLeft (currentShape: List[Point], anchorPoint: Int): List[Point] = {
//    val newShape = currentShape.map { point =>
//      val newX = point.y + anchorPoint
//      val newY = -point.x + anchorPoint + 1
//      Point(newX, newY)
//    }
//    newShape
//  }
//
//  override def rotateRight (currentShape: List[Point], anchorPoint: Int): List[Point] = {
//    val newShape = currentShape.map { point =>
//      val newX = -point.y + anchorPoint
//      val newY = point.x - anchorPoint + 2
//      Point(newX, newY)
//    }
//    newShape
//  }
//}
//
//class OCenteredTetrominos extends Tetromino {
//  override def rotateLeft(currentShape: List[Point], anchorPoint: Int): List[Point] = {
//    currentShape
//  }
//
//  override def rotateRight(currentShape: List[Point], anchorPoint: Int): List[Point] = {
//    currentShape
//  }
//}
//
//class ICenteredTetrominos extends Tetromino {
//  override def rotateLeft (currentShape: List[Point], anchorPoint: Int): List[Point] = {
//    currentShape
//  }
//
//  override def rotateRight(currentShape: List[Point], anchorPoint: Int): List[Point] = {
//    currentShape
//  }
//}


