package tetris.logic

import engine.graphics.Rectangle
import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

case class GameState (
                     gameIsOver: Boolean,
                     currentBlock: Tetromino,
                     currentBlockShape: List[Point],
                     currentBlockColor: CellType,
                     anchorPoint: Int,
                     tetrisBlocks: List[(CellType, List[Point])]
                     )

class TetrisLogic(val randomGen: RandomGenerator, val gridDims: Dimensions, val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims: Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  private var currentGameState: GameState = GameState(
    gameIsOver = false,
    currentBlock = null,
    currentBlockShape = List(),
    currentBlockColor = Empty,
    anchorPoint = 0,
    tetrisBlocks = List()
  )

  private val blockTypes: List[CellType] = List(ICell, JCell, LCell, OCell, SCell, TCell, ZCell)

  if (gridDims.width % 2 == 0) {
    currentGameState = currentGameState.copy(anchorPoint = gridDims.width / 2)
  } else {
    currentGameState = currentGameState.copy(anchorPoint = (gridDims.width / 2) + 1)
  }

  def createBlock(): Unit = {
    val randomShapeIndex = randomGen.randomInt(blockTypes.length)
    val randomShape = blockTypes(randomShapeIndex)

    randomShape match {
      case ICell => currentGameState = currentGameState.copy(
          currentBlock = new ICellBlock(currentGameState.anchorPoint),
          currentBlockShape = new ICellBlock(currentGameState.anchorPoint).initialPositions(currentGameState),
          currentBlockColor = ICell
        )
      case JCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new JCellBlock(currentGameState.anchorPoint),
          currentBlockShape = new JCellBlock(currentGameState.anchorPoint).initialPositions(currentGameState),
          currentBlockColor = JCell
        )
      case LCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new LCellBlock(currentGameState.anchorPoint),
          currentBlockShape = new LCellBlock(currentGameState.anchorPoint).initialPositions(currentGameState),
          currentBlockColor = LCell
        )
      case OCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new OCellBlock(currentGameState.anchorPoint),
          currentBlockShape = new OCellBlock(currentGameState.anchorPoint).initialPositions(currentGameState),
          currentBlockColor = OCell
        )
      case SCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new SCellBlock(currentGameState.anchorPoint),
          currentBlockShape = new SCellBlock(currentGameState.anchorPoint).initialPositions(currentGameState),
          currentBlockColor = SCell
        )
      case TCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new TCellBlock(currentGameState.anchorPoint),
          currentBlockShape = new TCellBlock(currentGameState.anchorPoint).initialPositions(currentGameState),
          currentBlockColor = TCell
        )
      case ZCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new ZCellBlock(currentGameState.anchorPoint),
          currentBlockShape = new ZCellBlock(currentGameState.anchorPoint).initialPositions(currentGameState),
          currentBlockColor = ZCell
        )
    }

  }

  def rotateLeft(): Unit = {
//    if (currentGameState.currentBlockColor != OCell) {
//      val newShape = currentGameState.currentBlockShape.map { point =>
//        val newX = point.y + currentGameState.anchorPoint
//        val newY = -point.x + currentGameState.anchorPoint + 1
//        Point(newX, newY)
//      }
//      currentGameState = currentGameState.copy(currentBlockShape = newShape)
//    }
    if (currentGameState.currentBlockColor == OCell) {
//      val currentTetromino = new OCenteredTetrominos
//      val rotatedTetromino = currentTetromino.rotateLeft(currentGameState.currentBlockShape, currentGameState.anchorPoint)
//      currentGameState = currentGameState.copy(currentBlockShape = rotatedTetromino)
    } else if (currentGameState.currentBlockColor == ICell) {
//      val currentTetromino = new ICenteredTetrominos
//      val rotatedTetromino = currentTetromino.rotateLeft(currentGameState.currentBlockShape, currentGameState.anchorPoint)
//      currentGameState = currentGameState.copy(currentBlockShape = rotatedTetromino)
    }  else {

    }
  }

  def rotateRight(): Unit = {
//    if (currentGameState.currentBlockColor != OCell) {
//      val newShape = currentGameState.currentBlockShape.map { point =>
//        val newX = -point.y + currentGameState.anchorPoint
//        val newY = point.x - currentGameState.anchorPoint + 2
//        Point(newX, newY)
//      }
//      currentGameState = currentGameState.copy(currentBlockShape = newShape)
//    }
    if (currentGameState.currentBlockColor == OCell) {

    } else if (currentGameState.currentBlockColor == ICell) {

    } else {

    }
  }

  def moveLeft(): Unit = {
    val lowestBlockWidth = currentGameState.currentBlockShape.map(_.x).min // Returns the x-coordinate of the block with the lowest value
    if (lowestBlockWidth > 0) {
      currentGameState = currentGameState.copy(currentBlockShape = currentGameState.currentBlockShape.map(point => point.copy(x = point.x - 1)))
    }
  }

  def moveRight(): Unit = {
    val highestBlockWidth = currentGameState.currentBlockShape.map(_.x).max // Returns the x-coordinate of the block with the highest value
    if (highestBlockWidth + 1 < gridDims.width) {
      currentGameState = currentGameState.copy(currentBlockShape = currentGameState.currentBlockShape.map(point => point.copy(x = point.x + 1)))
    }
  }

  def canMoveDown(gameState: GameState): Boolean = {
    val highestBlockHeight = gameState.currentBlockShape.map(_.y).max // Returns the y-coordinate of the block with the highest value
    val potentialNewPositions = gameState.currentBlockShape.map(point => point.copy(y = point.y + 1)) // Check if moving down would collide with existing blocks
    val collidesWithExistingBlocks = potentialNewPositions.exists(newPosition =>
      gameState.tetrisBlocks.exists(existingBlock =>
        existingBlock._2.contains(newPosition)
      )
    )
    highestBlockHeight + 1 < gridDims.height && !collidesWithExistingBlocks
  }

  def moveDown(): Unit = {
    if (canMoveDown(currentGameState)) {
      currentGameState = currentGameState.copy(currentBlockShape = currentGameState.currentBlockShape.map(point => point.copy(y = point.y + 1)))
    } else {
      val newTetrisBlock = (currentGameState.currentBlockColor, currentGameState.currentBlockShape)
      val newTetrisBlocks = newTetrisBlock :: currentGameState.tetrisBlocks
      currentGameState = currentGameState.copy(tetrisBlocks = newTetrisBlocks)
      createBlock()
    }
  }

  def doHardDrop(): Unit = {
    while (canMoveDown(currentGameState)) {
      moveDown()
    }
  }

  def isGameOver: Boolean = currentGameState.gameIsOver

  def getCellType(p: Point): CellType = {

    if (currentGameState.currentBlock == null) {
      createBlock()
    }

    if (currentGameState.currentBlockShape.contains(p)) {
      return currentGameState.currentBlockColor
    }

    for (previousBlocks <- currentGameState.tetrisBlocks) {
      val previousBlockColor = previousBlocks._1
      val previousBlockShape = previousBlocks._2
      if (previousBlockShape.contains(p)) {
        return previousBlockColor
      }
    }

    Empty
  }

}

object TetrisLogic {

  val FramesPerSecond: Int = 1
  val DrawSizeFactor = 1.0

  def makeEmptyBoard(gridDims: Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }

  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims: Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)

  def apply(): TetrisLogic = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))
}