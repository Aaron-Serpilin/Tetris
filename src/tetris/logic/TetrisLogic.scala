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
                     tetrisBlocks: List[Tetromino]
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
  private val tetrisBlocks: List[Tetromino] = List()

  if (gridDims.width % 2 == 0) {
    currentGameState = currentGameState.copy(anchorPoint = gridDims.width / 2)
  } else {
    currentGameState = currentGameState.copy(anchorPoint = (gridDims.width / 2) + 1)
  }

  //private val anchorPosition = Point(anchorPoint - 1, 1)

  def createBlock(): Unit = {
    val randomShapeIndex = randomGen.randomInt(blockTypes.length)
    val randomShape = blockTypes(randomShapeIndex)

    randomShape match {
      case ICell => currentGameState = currentGameState.copy(
          currentBlock = new ICellBlock(currentGameState.anchorPoint),
          currentBlockShape = List(Point(currentGameState.anchorPoint - 2, 1), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1), Point(currentGameState.anchorPoint + 1, 1)),
          currentBlockColor = ICell
        )
      case JCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new JCellBlock(currentGameState.anchorPoint),
          currentBlockShape = List(Point(currentGameState.anchorPoint - 2, 0), Point(currentGameState.anchorPoint - 2, 1), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1)),
          currentBlockColor = JCell
        )
      case LCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new LCellBlock(currentGameState.anchorPoint),
          currentBlockShape = List(Point(currentGameState.anchorPoint - 2, 1), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1), Point(currentGameState.anchorPoint, 0)),
          currentBlockColor = LCell
        )
      case OCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new OCellBlock(currentGameState.anchorPoint),
          currentBlockShape = List(Point(currentGameState.anchorPoint - 1, 0), Point(currentGameState.anchorPoint, 0), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1)),
          currentBlockColor = OCell
        )
      case SCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new SCellBlock(currentGameState.anchorPoint),
          currentBlockShape = List(Point(currentGameState.anchorPoint - 2, 1), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint - 1, 0), Point(currentGameState.anchorPoint, 0)),
          currentBlockColor = SCell
        )
      case TCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new TCellBlock(currentGameState.anchorPoint),
          currentBlockShape = List(Point(currentGameState.anchorPoint - 2, 1), Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1), Point(currentGameState.anchorPoint - 1, 0)),
          currentBlockColor = TCell
        )
      case ZCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new ZCellBlock(currentGameState.anchorPoint),
          currentBlockShape = List(Point(currentGameState.anchorPoint - 1, 1), Point(currentGameState.anchorPoint, 1), Point(currentGameState.anchorPoint - 1, 0), Point(currentGameState.anchorPoint - 2, 0)),
          currentBlockColor = ZCell
        )
    }

  }

  def rotateLeft(): Unit = {
//
//    if (currentGameState.currentBlockColor != OCell) {
//      val newShape = currentGameState.currentBlockShape.map { point =>
//        val newX = point.y + currentGameState.anchorPoint
//        val newY = -point.x + currentGameState.anchorPoint + 1
//        Point(newX, newY)
//      }
//      currentGameState = currentGameState.copy(currentBlockShape = newShape)
//    }
  }

  def rotateRight(): Unit = {
//
//    if (currentGameState.currentBlockColor != OCell) {
//      val newShape = currentGameState.currentBlockShape.map { point =>
//        val newX = -point.y + currentGameState.anchorPoint
//        val newY = point.x - currentGameState.anchorPoint + 2
//        Point(newX, newY)
//      }
//      currentGameState = currentGameState.copy(currentBlockShape = newShape)
//    }
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

  private def canMoveDown(): Boolean = {
    val highestBlockHeight = currentGameState.currentBlockShape.map(_.y).max // Returns the y-coordinate of the block with the highest value
    val potentialNewPositions = currentGameState.currentBlockShape.map(point => point.copy(y = point.y + 1)) // Check if moving down would collide with existing blocks
//    val collidesWithExistingBlocks = potentialNewPositions.exists(newPosition =>
//      currentGameState.tetrisBlocks.exists(existingBlock =>
//        existingBlock.shape.contains(newPosition)
//      )
//    )

    highestBlockHeight + 1 < gridDims.height //&& !collidesWithExistingBlocks
  }

  def moveDown(): Unit = {

    if (canMoveDown()) {
      currentGameState = currentGameState.copy(currentBlockShape = currentGameState.currentBlockShape.map(point => point.copy(y = point.y + 1)))
    } else {
      currentGameState = currentGameState.copy(tetrisBlocks = currentGameState.currentBlock +: currentGameState.tetrisBlocks) // We store all the blocks in a List once they've reached their final positions
      createBlock()
    }

  }

  def doHardDrop(): Unit = {
    while (canMoveDown()) {
      moveDown()
    }
  }

  def isGameOver: Boolean = currentGameState.gameIsOver

  def getAllBlocks: List[Tetromino] = currentGameState.currentBlock +: currentGameState.tetrisBlocks

  def getCellType(p: Point): CellType = {

    if (currentGameState.currentBlock == null) {
      createBlock()
    }

    if (currentGameState.currentBlockShape.contains(p)) {
      return currentGameState.currentBlockColor
    }

//    for (block <- currentGameState.tetrisBlocks) {
//      if (block.shape.contains(p)) {
//        return block.color
//      }
//    }

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