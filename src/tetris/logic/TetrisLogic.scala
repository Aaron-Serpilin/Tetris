package tetris.logic

//import engine.graphics.Rectangle
import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.TetrisLogic._

case class GameState (
                     gameIsOver: Boolean,
                     currentBlock: Tetromino,
                     absoluteBlockShape: List[Point],
                     relativeBlockShape: List[Point],
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
    absoluteBlockShape = List(),
    relativeBlockShape = List(),
    currentBlockType = Empty,
    anchorPosition = Point(0, 0),
    tetrisBlocks = List()
  )

  private val blockTypes: List[CellType] = List(ICell, JCell, LCell, OCell, SCell, TCell, ZCell)

  if (gridDims.width % 2 == 0) currentGameState = currentGameState.copy(anchorPosition = Point((gridDims.width / 2) - 1, 1))
  else currentGameState = currentGameState.copy(anchorPosition = Point(gridDims.width / 2, 1))


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
    val tetromino = randomShape match {
      case ICell => new ICellBlock(currentGameState)
      case OCell => new OCellBlock(currentGameState)
      case JCell | LCell | SCell | TCell | ZCell => new standardBlock(currentGameState, randomShape)
    }

    val relativePositions = tetromino.initialPositions()
    val absolutePositions = relativeToAbsolutePositions(relativePositions)

    currentGameState = currentGameState.copy(
      currentBlock = tetromino,
      relativeBlockShape = relativePositions,
      absoluteBlockShape = absolutePositions,
      currentBlockType = randomShape
    )
  }

  def rotate(direction: String): Unit = {
    val currentTetromino = currentGameState.currentBlockType match {
      case OCell => new OCellBlock(currentGameState)
      case ICell => new ICellBlock(currentGameState)
      case _ => new standardBlock(currentGameState, currentGameState.currentBlockType)
    }

    val relativeRotatedTetromino = if (direction == "L") currentTetromino.rotateLeft() else currentTetromino.rotateRight()
    val rotatedTetromino = relativeToAbsolutePositions(relativeRotatedTetromino)

    currentGameState = currentGameState.copy(
      relativeBlockShape = relativeRotatedTetromino,
      absoluteBlockShape = rotatedTetromino
    )
  }

  def rotateLeft(): Unit = rotate("L")

  def rotateRight(): Unit = rotate("R")

  def moveLeft(): Unit = {
    val lowestBlockWidth = currentGameState.absoluteBlockShape.map(_.x).min // Returns the x-coordinate of the block with the lowest value
    if (lowestBlockWidth > 0) {
      currentGameState = currentGameState.copy(
        absoluteBlockShape = currentGameState.absoluteBlockShape.map(point => point.copy(x = point.x - 1)),
        anchorPosition = Point(currentGameState.anchorPosition.x - 1, currentGameState.anchorPosition.y)
      )
    }
  }

  def moveRight(): Unit = {
    val highestBlockWidth = currentGameState.absoluteBlockShape.map(_.x).max // Returns the x-coordinate of the block with the highest value
    if (highestBlockWidth + 1 < gridDims.width) {
      currentGameState = currentGameState.copy(
        absoluteBlockShape = currentGameState.absoluteBlockShape.map(point => point.copy(x = point.x + 1)),
        anchorPosition = Point(currentGameState.anchorPosition.x + 1, currentGameState.anchorPosition.y)
      )
    }
  }

  def canMoveDown(): Boolean = {
    val highestBlockHeight = currentGameState.absoluteBlockShape.map(_.y).max // Returns the y-coordinate of the block with the highest value
    val potentialNewPositions = currentGameState.absoluteBlockShape.map(point => point.copy(y = point.y + 1)) // Check if moving down would collide with existing blocks
    val collidesWithExistingBlocks = potentialNewPositions.exists(newPosition =>
      currentGameState.tetrisBlocks.exists(existingBlock =>
        existingBlock._2.contains(newPosition)
      )
    )
    highestBlockHeight + 1 < gridDims.height && !collidesWithExistingBlocks
  }

  def moveDown(): Unit = {
    if (canMoveDown()) {
      currentGameState = currentGameState.copy(
        absoluteBlockShape = currentGameState.absoluteBlockShape.map(point => point.copy(y = point.y + 1)),
        anchorPosition = Point(currentGameState.anchorPosition.x, currentGameState.anchorPosition.y + 1)
      )
    } else {
      val newTetrisBlock = (currentGameState.currentBlockType, currentGameState.absoluteBlockShape)
      val newTetrisBlocks = newTetrisBlock :: currentGameState.tetrisBlocks
      currentGameState = currentGameState.copy(tetrisBlocks = newTetrisBlocks)

      if (gridDims.width % 2 == 0) currentGameState = currentGameState.copy(anchorPosition = Point(gridDims.width / 2, 1))
      else currentGameState = currentGameState.copy(anchorPosition = Point((gridDims.width / 2) + 1, 1))

      createBlock()
    }
  }

  def doHardDrop(): Unit = {
    while (canMoveDown()) {moveDown()}
  }

  def isGameOver: Boolean = currentGameState.gameIsOver

  def readInitialBoard(initialBoard: Seq[Seq[CellType]]): Unit = {
    val tetrisBlocks = initialBoard.zipWithIndex.flatMap {
      case (row, rowIndex) =>
        row.zipWithIndex.collect {
          case (cellType, columnIndex) if cellType != Empty =>
            val absolutePosition = Point(columnIndex, rowIndex)
            (cellType, List(absolutePosition))
        }
    }.toList

    currentGameState = currentGameState.copy(tetrisBlocks = tetrisBlocks)
  }


  def getCellType(p: Point): CellType = {

    readInitialBoard(initialBoard)
    if (currentGameState.currentBlock == null) createBlock()
    if (currentGameState.absoluteBlockShape.contains(p)) return currentGameState.currentBlockType

    for (previousBlocks <- currentGameState.tetrisBlocks) {
      val previousBlockColor = previousBlocks._1
      val previousBlockShape = previousBlocks._2
      if (previousBlockShape.contains(p)) return previousBlockColor
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
