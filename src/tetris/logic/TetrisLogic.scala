package tetris.logic

import engine.graphics.Rectangle
import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

case class GameState (
                     gameIsOver: Boolean,
                     currentBlock: Tetromino,
                     currentBlockShape: List[Point],
                     currentBlockType: CellType,
                     anchorPosition: Point,
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
    currentBlockType = Empty,
    anchorPosition = Point(0, 0),
    tetrisBlocks = List()
  )

  private val blockTypes: List[CellType] = List(ICell, JCell, LCell, OCell, SCell, TCell, ZCell)

  if (gridDims.width % 2 == 0) {
    currentGameState = currentGameState.copy(anchorPosition = Point((gridDims.width / 2) - 1, 1))
  } else {
    currentGameState = currentGameState.copy(anchorPosition = Point(gridDims.width / 2, 1))
  }

  def relativeToAbsolutePositions (blockShape: List[Point]): List[Point] = {
    val adaptedShape = blockShape.map{point =>
      val absoluteX = point.x + currentGameState.anchorPosition.x
      val absoluteY = point.y + currentGameState.anchorPosition.y
      Point(absoluteX, absoluteY)
    }
    adaptedShape
  }

  def createBlock(): Unit = {
    val randomShapeIndex = randomGen.randomInt(blockTypes.length)
    val randomShape = blockTypes(randomShapeIndex)

    randomShape match {
      case ICell => currentGameState = currentGameState.copy(
          currentBlock = new ICellBlock(currentGameState),
          currentBlockShape = relativeToAbsolutePositions(new ICellBlock(currentGameState).initialPositions()),
          currentBlockType = ICell
        )
      case OCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new OCellBlock(currentGameState),
          currentBlockShape = relativeToAbsolutePositions(new OCellBlock(currentGameState).initialPositions()),
          currentBlockType = OCell
        )
      case JCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new standardBlock(currentGameState, JCell),
          currentBlockShape = relativeToAbsolutePositions(new standardBlock(currentGameState, JCell).initialPositions()),
          currentBlockType = JCell
        )
      case LCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new standardBlock(currentGameState, LCell),
          currentBlockShape = relativeToAbsolutePositions(new standardBlock(currentGameState, LCell).initialPositions()),
          currentBlockType = LCell
        )
      case SCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new standardBlock(currentGameState, SCell),
          currentBlockShape = relativeToAbsolutePositions(new standardBlock(currentGameState, SCell).initialPositions()),
          currentBlockType = SCell
        )
      case TCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new standardBlock(currentGameState, TCell),
          currentBlockShape = relativeToAbsolutePositions(new standardBlock(currentGameState, TCell).initialPositions()),
          currentBlockType = TCell
        )
      case ZCell =>
        currentGameState = currentGameState.copy(
          currentBlock = new standardBlock(currentGameState, ZCell),
          currentBlockShape = relativeToAbsolutePositions(new standardBlock(currentGameState, ZCell).initialPositions()),
          currentBlockType = ZCell
        )
    }

  }

  def rotateLeft(): Unit = {

    currentGameState.currentBlockType match {
      case OCell =>
        val currentTetromino = new OCellBlock(currentGameState)
        val rotatedTetromino = currentTetromino.rotateLeft()
        currentGameState = currentGameState.copy(currentBlockShape = rotatedTetromino)

      case ICell =>
        val currentTetromino = new ICellBlock(currentGameState)
        val rotatedTetromino = currentTetromino.rotateLeft()
        currentGameState = currentGameState.copy(currentBlockShape = rotatedTetromino)

      case default =>
        val currentTetromino = new standardBlock(currentGameState, currentGameState.currentBlockType)
        val rotatedTetromino = currentTetromino.rotateLeft()
        currentGameState = currentGameState.copy(currentBlockShape = rotatedTetromino)
    }
  }


  def rotateRight(): Unit = {

    currentGameState.currentBlockType match {
      case OCell =>
        val currentTetromino = new OCellBlock(currentGameState)
        val rotatedTetromino = currentTetromino.rotateRight()
        currentGameState = currentGameState.copy(currentBlockShape = rotatedTetromino)

      case ICell =>
        val currentTetromino = new ICellBlock(currentGameState)
        val rotatedTetromino = currentTetromino.rotateRight()
        currentGameState = currentGameState.copy(currentBlockShape = rotatedTetromino)

      case default =>
        val currentTetromino = new standardBlock(currentGameState, currentGameState.currentBlockType)
        val rotatedTetromino = currentTetromino.rotateRight()
        currentGameState = currentGameState.copy(currentBlockShape = rotatedTetromino)
    }
  }

  def moveLeft(): Unit = {
    val lowestBlockWidth = currentGameState.currentBlockShape.map(_.x).min // Returns the x-coordinate of the block with the lowest value
    if (lowestBlockWidth > 0) {
      currentGameState = currentGameState.copy(
        currentBlockShape = currentGameState.currentBlockShape.map(point => point.copy(x = point.x - 1)),
        anchorPosition = Point(currentGameState.anchorPosition.x - 1, currentGameState.anchorPosition.y)
      )
    }
  }

  def moveRight(): Unit = {
    val highestBlockWidth = currentGameState.currentBlockShape.map(_.x).max // Returns the x-coordinate of the block with the highest value
    if (highestBlockWidth + 1 < gridDims.width) {
      currentGameState = currentGameState.copy(
        currentBlockShape = currentGameState.currentBlockShape.map(point => point.copy(x = point.x + 1)),
        anchorPosition = Point(currentGameState.anchorPosition.x + 1, currentGameState.anchorPosition.y)
      )
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
      currentGameState = currentGameState.copy(
        currentBlockShape = currentGameState.currentBlockShape.map(point => point.copy(y = point.y + 1)),
        anchorPosition = Point(currentGameState.anchorPosition.x, currentGameState.anchorPosition.y + 1)
      )
    } else {
      val newTetrisBlock = (currentGameState.currentBlockType, currentGameState.currentBlockShape)
      val newTetrisBlocks = newTetrisBlock :: currentGameState.tetrisBlocks
      currentGameState = currentGameState.copy(
        tetrisBlocks = newTetrisBlocks
      )

      if (gridDims.width % 2 == 0) {
        currentGameState = currentGameState.copy(anchorPosition = Point(gridDims.width / 2, 1))
      } else {
        currentGameState = currentGameState.copy(anchorPosition = Point((gridDims.width / 2) + 1, 1))
      }

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
      return currentGameState.currentBlockType
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

//override def initialPositions(): List[Point] = {
//  blockType match {
//    case JCell => List(Point(currentGameState.anchorPosition.x - 2, 0), Point(currentGameState.anchorPosition.x - 2, 1), Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x, 1))
//    case LCell => List(Point(currentGameState.anchorPosition.x - 2, 1), Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x, 1), Point(currentGameState.anchorPosition.x, 0))
//    case SCell => List(Point(currentGameState.anchorPosition.x - 2, 1), Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x - 1, 0), Point(currentGameState.anchorPosition.x, 0))
//    case TCell => List(Point(currentGameState.anchorPosition.x - 2, 1), Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x, 1), Point(currentGameState.anchorPosition.x - 1, 0))
//    case ZCell => List(Point(currentGameState.anchorPosition.x - 1, 1), Point(currentGameState.anchorPosition.x, 1), Point(currentGameState.anchorPosition.x - 1, 0), Point(currentGameState.anchorPosition.x - 2, 0))
//  }
//}