package tetris.logic

abstract class Tetromino() {
  def rotateLeft(): Unit
  def rotateRight(): Unit
}

class ICellBlock(anchorPoint: Int) extends Tetromino {

  override def rotateLeft(): Unit = {

  }

  override def rotateRight(): Unit = {

  }

}

class JCellBlock(anchorPoint: Int) extends Tetromino {

  override def rotateLeft(): Unit = {

  }

  override def rotateRight(): Unit = {

  }

}

class LCellBlock(anchorPoint: Int) extends Tetromino {

  override def rotateLeft(): Unit = {

  }

  override def rotateRight(): Unit = {

  }
}

class OCellBlock(anchorPoint: Int) extends Tetromino {

  override def rotateLeft(): Unit = {

  }

  override def rotateRight(): Unit = {

  }
}

class SCellBlock(anchorPoint: Int) extends Tetromino {

  override def rotateLeft(): Unit = {

  }

  override def rotateRight(): Unit = {

  }
}

class TCellBlock(anchorPoint: Int) extends Tetromino {

  override def rotateLeft(): Unit = {

  }

  override def rotateRight(): Unit = {

  }
}

class ZCellBlock(anchorPoint: Int) extends Tetromino {

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
//  override def rotateLeft (currentShape: List[Point], anchorPoint: Int): List[Point] = {
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
//    // IBlock rotation functionality
//  }
//
//  override def rotateRight(currentShape: List[Point], anchorPoint: Int): List[Point] = {
//    // IBlock rotation functionality
//  }
//}


